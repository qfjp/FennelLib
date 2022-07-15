(local List {})
(fn List.new [self]
  (setmetatable {:_head nil :_tail nil}
                {:__index self :__tostring (. self :toStr)}))

(fn List.new [self head]
  (setmetatable {:_head head :_tail nil}
                {:__index self :__tostring (. self :toStr)}))

(fn List.new [self head tail]
  (setmetatable {:_head head :_tail tail}
                {:__index self :__tostring (. self :toStr)}))

(fn List.push [self item ...]
  (if (= nil ...)
      (let [head self._head
            tail self._tail]
        (List:new item self))
      (: (List:new item self) :push ...)))

(fn List.pop [self]
  (self:tail))

(fn List.head [self]
  self._head)

(fn List.tail [self]
  self._tail)

(fn List._toStr [self]
  (let [head self._head
        tail self._tail]
    (if (= head nil) ""
        (if (= tail._tail nil)
            (.. "" head)
            (.. "" head ", " (tail:_toStr))))))

(fn List.toStr [self]
  (.. "[" (List._toStr self) "]"))

(fn List.print [self]
  (print (self:toStr)))

List

