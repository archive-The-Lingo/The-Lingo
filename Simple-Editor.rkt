#lang racket
{require racket/gui/base}
{require "The-Lingo.rkt"}

{define (init)
  {define the-frame
    {new frame% [label "Simple Edit"]
         [width 200]
         [height 200]}}
  {define the-canvas
    {new
     {class editor-canvas%
       {define/override (on-char event)
         (on-key-event event)}
       {super-new}}
     [parent the-frame]}}
  {define the-text {new text%}}
  {send the-canvas set-editor the-text}

  {define key/c
    (or/c
     'shift
     'control
     'home
     'end

     'space
     'return
     'tab
     'backspace
     'delete
     (and/c char? (not/c (or/c #\space #\return #\tab #\backspace #\rubout))))}

  {define key-stack (box '())}
  {define/contract (char-stack-push! ch)
    (-> key/c void?)
    {let* ([old (unbox key-stack)] [new (cons ch old)])
      {if (box-cas! key-stack old new)
          (void)
          (char-stack-push! ch)}}}
  {define/contract (char-stack-pop!)
    (-> (or/c key/c void?))
    {let ([old (unbox key-stack)])
      {if (null? old)
          (void)
          {let ([result (car old)] [new (cdr old)])
            {if (box-cas! key-stack old new)
                result
                (char-stack-pop!)}}}}}

  {define screen (box "")}

  {define/contract (on-key-event the-key)
    (-> (is-a?/c key-event%) void?)
    {let ([x (key-event->key the-key)])
      {if (void? x)
          (void)
          {begin
            (char-stack-push! x)
            (on-key)}}}}
  {define/contract (key-event->key the-key)
    (-> (is-a?/c key-event%) (or/c key/c void?))
    {let ([key-code (send the-key get-key-code)])
      {if (symbol? key-code)
          {cond
            [(eq? 'release key-code) (void)]
            [(or (eq? key-code 'shift) (eq? key-code 'rshift)) 'shift]
            [(or (eq? key-code 'control) (eq? key-code 'rcontrol)) 'control]
            [(set-member? (set 'home 'end) key-code) key-code]
            [else (void)]}
          {cond
            [(eq? #\space key-code) 'space]
            [(eq? #\return key-code) 'return]
            [(eq? #\tab key-code) 'tab]
            [(eq? #\backspace key-code) 'backspace]
            [(eq? #\rubout key-code) 'delete]
            [else key-code]}}}}
  {define/contract (on-key)
    (-> void?)
    (writeln (unbox key-stack))}
  {send the-frame show #t}
  }