import IO from "core/io";
import List from "core/list";

# you know what it is
def map(f, xs) -> {
  let recur = (ys, zs) -> {
    if List.empty?(ys) {
      zs;
    } else {
      recur(List.tail(ys), List.snoc(zn, f(List.head(ys))));
    }
  };

  recur(xs, []);
};

let inc_all = map(x -> x + 1);

let list = [1, 2, 3];

if List.length(list) == 3 && List.foldl((a, e) -> a + e, 0, list) == 6 {
  IO[:puts](inc_all(list));
}

{
  big: "ol",
  "friggen" => HASH,
  4 => 20,
  function: () -> 1,
  "json": rocks
};

begin {
  let file = IO.open("test.txt");
} rescue fErr {
  IO.puts("type: " + fErr.type + ", message: " + fErr.message);
}

def with_file(path, f) -> {
  let file = IO.open(path);

  f(file);

  file.close();
} rescue err {
  IO.puts(":/ sorry i couldn't open it cause of: " + err);
};
