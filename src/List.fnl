;; Immutable lists and associated functions
(local list {})
(fn list.new [self]
  (setmetatable {:_head nil :_tail nil}
                {:__index self :__tostring (. self :toStr)}))

(fn list.new [self head]
  (setmetatable {:_head head :_tail nil}
                {:__index self :__tostring (. self :toStr)}))

(fn list.new [self head tail]
  (setmetatable {:_head head :_tail tail}
                {:__index self :__tostring (. self :toStr)}))

(fn list.push [self item ...]
  (if (= nil ...)
      (let [head self._head
            tail self._tail]
        (list:new item self))
      (: (list:new item self) :push ...)))

(fn list.pop [self]
  (self:tail))

(fn list.head [self]
  self._head)

(fn list.tail [self]
  self._tail)

(fn list._toStr [self]
  (let [head self._head
        tail self._tail]
    (if (= head nil) ""
        (if (= tail._tail nil)
            (.. "" head)
            (.. "" head ", " (tail:_toStr))))))

(fn list.toStr [self]
  (.. "[" (list._toStr self) "]"))

(fn list.print [self]
  (print (self:toStr)))

list

