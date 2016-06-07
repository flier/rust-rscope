use std::path::{Path, PathBuf};

use cargo::core::{Package, PackageSet};
use cargo::ops::resolve_dependencies;
use cargo::util::Config;

use errors::Result;

pub fn load_crate<'a>(dir: &Path, conf: &'a Config) -> Result<(Package, PackageSet<'a>)> {
    let mut manifest_path = PathBuf::from(dir);

    manifest_path.push("Cargo.toml");

    debug!("loading crate; manifest-path={}", manifest_path.display());

    let root_package = try!(Package::for_path(&manifest_path, conf));

    info!("loaded package; package={}", root_package);

    for key in root_package.manifest().warnings().iter() {
        warn!("{}", key)
    }

    let (packages, _) = {
        try!(resolve_dependencies(&root_package, conf, None, Vec::new(), true))
    };

    Ok((root_package, packages))
}
