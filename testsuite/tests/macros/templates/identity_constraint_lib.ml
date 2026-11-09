module type S = sig val x : int end
module T $(G : S) = struct include G end
