// libgit2 uses Win32 security/registry APIs that are provided by advapi32.
// Link it explicitly so Windows builds don't rely on transitive system libs.
#[cfg(windows)]
#[link(name = "advapi32")]
unsafe extern "system" {}

pub mod files;
#[cfg(not(target_arch = "wasm32"))]
pub mod git;
#[cfg(target_arch = "wasm32")]
pub mod git_stub;
#[cfg(target_arch = "wasm32")]
pub use git_stub as git;
pub mod graph;
pub mod ingot;
pub mod workspace;

pub trait Resolver: Sized {
    type Description;
    type Resource;
    type Error;
    type Diagnostic;
    type Event;

    fn resolve<H>(
        &mut self,
        handler: &mut H,
        description: &Self::Description,
    ) -> Result<H::Item, Self::Error>
    where
        H: ResolutionHandler<Self>;
}

pub trait ResolutionHandler<R>
where
    R: Resolver,
{
    type Item;

    fn on_resolution_diagnostic(&mut self, _diagnostic: R::Diagnostic) {}

    fn on_resolution_event(&mut self, _event: R::Event) {}

    fn on_resolution_error(&mut self, _description: &R::Description, _error: R::Error) {}

    fn handle_resolution(
        &mut self,
        description: &R::Description,
        resource: R::Resource,
    ) -> Self::Item;
}
