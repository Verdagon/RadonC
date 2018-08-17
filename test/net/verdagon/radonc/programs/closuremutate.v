
fn main() int {
	let $x = 4;
	{
		mut x = x + 1
	}();
	x
}
