pub mod files;
#[cfg(not(target_arch = "wasm32"))]
pub mod git;
#[cfg(target_arch = "wasm32")]
pub mod git_stub;
#[cfg(target_arch = "wasm32")]
pub use git_stub as git;
pub mod graph;
pub mod ingot;

pub trait Resolver: Sized {
    type Description;
    type Resource;
    type Error;
    type Diagnostic;

    fn resolve<H>(
        &mut self,
        handler: &mut H,
        description: &Self::Description,
    ) -> Result<H::Item, Self::Error>
    where
        H: ResolutionHandler<Self>;

    fn take_diagnostics(&mut self) -> Vec<Self::Diagnostic>;
}

pub trait ResolutionHandler<R>
where
    R: Resolver,
{
    type Item;

    fn handle_resolution(
        &mut self,
        description: &R::Description,
        resource: R::Resource,
    ) -> Self::Item;
}
