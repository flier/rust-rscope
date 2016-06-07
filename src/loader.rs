use std::fmt;
use std::path::{Path, PathBuf};

use cargo::core::{Package, PackageSet, Target, TargetKind};
use cargo::ops::resolve_dependencies;
use cargo::util::Config;

use errors::Result;

pub struct Task {
    pub base_dir: PathBuf,
    pub target: Target,
    pub is_dep: bool,
}

impl fmt::Debug for Task {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f,
               "task name={}, kind={}, src_path={}",
               self.target.name(),
               match *self.target.kind() {
                   TargetKind::Lib(_) => "lib",
                   TargetKind::CustomBuild => "build",
                   TargetKind::Bench => "bench",
                   TargetKind::Test => "test",
                   TargetKind::Example => "example",
                   TargetKind::Bin => "bin",
               },
               self.target.src_path().to_str().unwrap())
    }
}

impl Task {
    pub fn src_path(&self) -> PathBuf {
        if self.target.src_path().is_absolute() {
            PathBuf::from(self.target.src_path())
        } else {
            let mut p = self.base_dir.clone();

            p.push(self.target.src_path());

            p
        }
    }
}

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

pub fn find_targets<'a>(pkg: &Package, deps: &PackageSet<'a>) -> Result<Vec<Task>> {
    let mut targets: Vec<Task> = pkg.targets()
        .iter()
        .map(|target| {
            Task {
                base_dir: PathBuf::from(pkg.root()),
                target: target.clone(),
                is_dep: false,
            }
        })
        .collect();

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
            .map(|target| {
                Task {
                    base_dir: PathBuf::from(dep.root()),
                    target: target.clone(),
                    is_dep: true,
                }
            }));
    }

    Ok(targets)
}
