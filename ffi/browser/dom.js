

(with-id tab.id
  (with-cache tab html-tab))


HTML

HTML-ID

(DEFINE-TYPE HTML-Unique
  (*text Text)
  (*html Text HTML))

(DEFINE-TYPE HTML-Attribute
  (*))

(DEFINE-TYPE HTML
  (*element Text (List HTML-Attribute) (List HTML-Unique)))

(PROTOCOL ($HTML a)
  ()
  )

text :: (-> Text HTML-Unique)
with-id :: (-> Text HTML HTML-Unique)
with-cache :: (-> a (-> a HTML) HTML)
element :: (-> Text (List Attribute) (List HTML-Unique) HTML)


(DEFINE html-tab :: (-> (-> Tab-Action (Task Null)) Tab HTML)
  (html-tab push tab)
    (with-id tab.id
      (with-cache tab -> tab
        (div [ (on-hover -> hover?
                 (push (*hover tab hover?)))
             | (on-hold -> hold?
                 (push (*hold tab hold?)))
             | (class class-tab-hover tab.hover?)
             | (class class-tab-hold tab.hold?) ]
          [ (html-tab-favicon tab)
          | (html-tab-text tab)
          | (html-tab-close tab) ]))))

(DEFINE html-group :: (-> (-> Group-Action (Task Null)) Group HTML)
  (html-group push group)
    (with-id group.id
      (with-cache group -> group
        (div [ (on-hover -> hover?
                 (push (*hover group hover?)))
             | (on-hold -> hold?
                 (push (*hold group hold?))) ]
          (LET push-tab = (-> value
                            (push (*tab-action value)))
            (map group.tabs -> tab
              (html-tab push-tab tab)))))))


(DEFINE-TYPE Tab
  (*tab { id :: Text
        | url :: (Mutable (Maybe Text))
        | title :: (Mutable (Maybe Text))
        | hovering? :: (Mutable Boolean)
        | holding? :: (Mutable Boolean)
        | hovering-close? :: (Mutable Boolean)
        | holding-close? :: (Mutable Boolean) }))

(DEFINE html-tab-favicon :: (-> Tab HTML)
  (html-tab-favicon (*tab tab))
  )

(DEFINE html-tab :: (-> Tab HTML)
  (html-tab (MATCH tab (*tab info)))
    (div [ (on-hover -> hover?
             (set! info.hovering? hover?))

         | (on-hold -> hold?
             (set! info.holding? hold?))

         | (toggle-style style-tab-hover
             info.hovering?)

         | (toggle-style style-tab-hold
             (and info.hovering?
                  info.holding?)) ]
      [ (html-tab-favicon tab)
      | (html-tab-text tab)
      | (html-tab-close tab) ]))

(DEFINE-STYLE style-tab-hover
  "background-color" = (always "red")
  "color" = (always "black"))

(DEFINE html-tab :: (-> Tab HTML)
  (html-tab (MATCH tab (*tab info)))
    (div { on-hover = (-> hover?
                        (set! info.hovering? hover?))

         | on-hold = (-> hold?
                       (set! info.holding? hold?))

         | styles = [ (toggle-style style-tab-hover
                        info.hovering?)
                    | (toggle-style style-tab-hold
                        (and info.hovering?
                             info.holding?))
                    | (inline-style "background-color"
                        (always "blue")) ] }
      [ (html-tab-favicon tab)
      | (html-tab-text tab)
      | (html-tab-close tab) ]))
