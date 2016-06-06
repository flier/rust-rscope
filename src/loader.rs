use std::path::Path;

use cargo::core::{Package, PackageSet};
use cargo::ops::resolve_dependencies;
use cargo::util::Config;

use errors::Result;

pub fn load<'a>(manifest_path: &Path, conf: &'a Config) -> Result<(Package, PackageSet<'a>)> {
    debug!("loading; manifest-path={}", manifest_path.display());

    let root_package = try!(Package::for_path(&manifest_path, conf));

    info!("loaded package; package={}", root_package);

    for key in root_package.manifest().warnings().iter() {
        warn!("{}", key)
    }

    let (packages, resolve_with_overrides) = {
        try!(resolve_dependencies(&root_package, conf, None, Vec::new(), true))
    };

    Ok((root_package, packages))
}
