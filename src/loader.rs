use std::path::{Path, PathBuf};

use cargo::core::{Package, PackageSet, Target};
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

pub fn find_targets<'a>(pkg: &Package, deps: &PackageSet<'a>) -> Result<Vec<(PathBuf, Target)>> {
    let mut targets: Vec<(PathBuf, Target)> =
        pkg.targets().iter().map(|target| (PathBuf::from(pkg.root()), target.clone())).collect();

    for dep in pkg.dependencies() {
        debug!("found dependency; name={}, version={}, locked={}",
               dep.name(),
               dep.version_req(),
               dep.specified_req().unwrap_or("N/A"));
    }

    for pkg_id in deps.package_ids() {
        let dep = try!(deps.get(pkg_id));

        info!("resolved package; {}, root={}",
              dep,
              dep.root().to_str().unwrap());

        targets.extend(dep.targets()
            .iter()
            .filter(|target| target.is_lib())
            .map(|target| (PathBuf::from(dep.root()), target.clone())));
    }

    Ok(targets)
}
