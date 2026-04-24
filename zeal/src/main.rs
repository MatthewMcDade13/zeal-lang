use kos::basic::{arena_static::ArenaStatic, static_sync::ArenaStaticSync};
use zeal_ast::Ast;

const PATH: &str = "../test_scripts/loops.zl";

#[global_allocator]
static G: ArenaStaticSync<1024> = ArenaStaticSync::<1024>::new();

fn main() -> anyhow::Result<()> {
    let mut x: Vec<i32> = Vec::new();
    x.extend_from_slice(&[1, 2, 3, 4, 5, 6, 7]);

    println!("{x:?}");
    let ast = Ast::from_file(PATH)?;
    // println!("{ast}");
    Ok(())
}
