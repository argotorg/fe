// Auto-follow toggle
function toggleAutoFollow(checked) {
    localStorage.setItem('fe-docs-auto-follow', checked);
}

// Go to source in editor (for LSP mode)
function gotoSource(path) {
    fetch('/api/goto/' + path, { method: 'POST' })
        .then(response => {
            if (!response.ok) {
                console.warn('Go to source failed:', response.status);
            }
        })
        .catch(err => {
            console.warn('Go to source error:', err);
        });
}

// Restore auto-follow state on page load
document.addEventListener('DOMContentLoaded', function() {
    const autoFollow = document.getElementById('auto-follow');
    if (autoFollow) {
        const saved = localStorage.getItem('fe-docs-auto-follow');
        autoFollow.checked = saved === 'true';
    }
});

// Search functionality
let searchTimeout;
function doSearch(query) {
    clearTimeout(searchTimeout);
    const results = document.getElementById('search-results');
    if (!query) {
        results.innerHTML = '';
        return;
    }
    searchTimeout = setTimeout(async () => {
        const resp = await fetch('/api/search?q=' + encodeURIComponent(query));
        const items = await resp.json();
        results.innerHTML = items.map(item =>
            `<a class="search-result" href="/doc/${item.path}/${item.kind}">
                <span class="kind-badge ${item.kind}">${item.kind}</span>
                ${item.name}
            </a>`
        ).join('');
    }, 150);
}

// WebSocket live reload
(function() {
    const wsProtocol = window.location.protocol === 'https:' ? 'wss:' : 'ws:';
    const ws = new WebSocket(wsProtocol + '//' + window.location.host + '/ws');

    ws.onmessage = function(event) {
        try {
            const msg = JSON.parse(event.data);
            if (msg.type === 'reload') {
                window.location.reload();
            } else if (msg.type === 'update') {
                // Preserve auto-follow checkbox state
                const autoFollow = document.getElementById('auto-follow');
                const wasChecked = autoFollow ? autoFollow.checked : false;

                // Re-fetch page and update both sidebar and content
                fetch(window.location.pathname)
                    .then(r => r.text())
                    .then(html => {
                        const parser = new DOMParser();
                        const newDoc = parser.parseFromString(html, 'text/html');
                        // Update main content
                        const newContent = newDoc.querySelector('.doc-content');
                        const oldContent = document.querySelector('.doc-content');
                        if (newContent && oldContent) {
                            oldContent.innerHTML = newContent.innerHTML;
                        }
                        // Update sidebar navigation
                        const newSidebar = newDoc.querySelector('.sidebar-nav');
                        const oldSidebar = document.querySelector('.sidebar-nav');
                        if (newSidebar && oldSidebar) {
                            oldSidebar.innerHTML = newSidebar.innerHTML;
                        }
                        // Restore checkbox state
                        const autoFollowAfter = document.getElementById('auto-follow');
                        if (autoFollowAfter) {
                            autoFollowAfter.checked = wasChecked;
                        }
                    });
            } else if (msg.type === 'navigate') {
                const currentPath = window.location.pathname.replace('/doc/', '');

                // Rename redirects (with if_on_path) always work if we're on the old page
                if (msg.if_on_path) {
                    if (currentPath === msg.if_on_path) {
                        window.location.href = '/doc/' + msg.path;
                    }
                    return; // Don't continue to auto-follow check for redirects
                }

                // Regular navigation only works with auto-follow enabled
                const autoFollow = document.getElementById('auto-follow');
                if (autoFollow && autoFollow.checked) {
                    window.location.href = '/doc/' + msg.path;
                }
            }
        } catch (e) {
            console.error('WebSocket message error:', e);
        }
    };

    ws.onclose = function() {
        // Attempt to reconnect after 2 seconds
        setTimeout(function() {
            window.location.reload();
        }, 2000);
    };
})();
