let rec findIndexInList = (list: list<'a>, id: 'a) => {
    switch list {
    | list{hd, ...tl} =>
      if hd == id {
        Some(0)
      } else {
        switch findIndexInList(tl, id) {
        | Some(i) => Some(i + 1)
        | None => None
        }
      }
    | list{} => None
    }
  }