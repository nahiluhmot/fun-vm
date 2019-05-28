import IO from "core/io";
import List from "core/list";

# you know what it is
def map(f, xs) => {
  let recur = (ys, zs) => {
    if List.empty?(ys) {
      zs;
    } else {
      recur(List.tail(ys), List.snoc(zn, f(List.head(ys))));
    }
  };

  recur(xs, []);
};

let inc_all = map(x => x + 1);

let list = [1,2,3];

IO[:puts](inc_all(list));
