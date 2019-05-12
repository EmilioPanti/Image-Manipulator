open System.Windows.Forms
open System.Drawing

type W2V() =
  let mutable w2v = new Drawing2D.Matrix()
  let mutable v2w = new Drawing2D.Matrix()

  member this.Translate(tx, ty) =
    w2v.Translate(tx, ty)
    v2w.Translate(-tx, -ty, Drawing2D.MatrixOrder.Append)

  member this.Rotate(a) =
    w2v.Rotate(a)
    v2w.Rotate(-a, Drawing2D.MatrixOrder.Append)

  member this.Scale(sx, sy) =
    w2v.Scale(sx, sy)
    v2w.Scale(1.f/sx, 1.f/sy, Drawing2D.MatrixOrder.Append)
  
  member this.W2V with get() = w2v and  set(m) = w2v <- m
  member this.V2W with get() = v2w and  set(m) = v2w <- m

let Norm (p:PointF) =
  sqrt(p.X*p.X+p.Y*p.Y)

let SubPointF (p:PointF,p2:PointF) =
  PointF(p.X-p2.X,p.Y-p2.Y)

let SumPointF (p:PointF,p2:PointF) =
  PointF(p.X+p2.X,p.Y+p2.Y)

let MulPointF (p:PointF,d:float32) =
  PointF(p.X*d,p.Y*d)

let Rect2RectF (r:Rectangle) =
  RectangleF(single r.X, single r.Y, single r.Width, single r.Height)

let RectF2Rect (r:RectangleF) =
  Rectangle(int r.X, int r.Y, int r.Width, int r.Height)

let PointsQuiteEqual (p1: PointF, p2: PointF, d: float32) =
  let x = p1.X - p2.X
  let y = p1.Y - p2.Y
  x < d && x > -d && y < d && y > -d

type CoordinateType = View | World

type LWControl() =
  let mutable coordinates = View

  let mutable position = PointF()
  let mutable size = SizeF()

  let mutable parent : Control = null

  let mousedownevt = new Event<MouseEventArgs>()
  let mousemoveevt = new Event<MouseEventArgs>()
  let mouseupevt = new Event<MouseEventArgs>()

  member this.CoordinateType
    with get() = coordinates
    and set(v) = coordinates <- v
 
  member this.Position
    with get() = position
    and set(v) = position <- v

  member this.Size
    with get() = size
    and set(v) = size <- v
  
  member this.Parent 
    with get() = parent
    and set(v) = parent <- v

  member this.MouseDown = mousedownevt.Publish

  abstract OnMouseDown : MouseEventArgs -> unit
  default this.OnMouseDown e = mousedownevt.Trigger(e)

  abstract OnMouseMove : MouseEventArgs -> unit
  default this.OnMouseMove e = mousemoveevt.Trigger(e)

  abstract OnMouseUp : MouseEventArgs -> unit
  default this.OnMouseUp e = mouseupevt.Trigger(e)

  abstract OnPaint : PaintEventArgs -> unit
  default this.OnPaint e = ()


  abstract HitTest : PointF -> bool
  default this.HitTest p =
    (new RectangleF(position, size)).Contains(p)

type LWContainer() as this =
  inherit UserControl()

  let mutable transform = W2V()

  let controls = ResizeArray<LWControl>()
  
  do
    this.SetStyle(ControlStyles.AllPaintingInWmPaint ||| ControlStyles.OptimizedDoubleBuffer, true)

  member this.Transform with get() = transform
                            and set(t) = transform <- t

  member this.TransformPoint (m:Drawing2D.Matrix) (p:PointF) =
    let pts = [| p |]
    m.TransformPoints(pts)
    pts.[0]

  member this.LWControls with get() = controls

  override this.OnMouseDown e =
    let p = PointF(single e.X, single e.Y)
    let controlsView = controls |> Seq.filter (fun c -> c.CoordinateType = View)
    match (controlsView |> Seq.tryFind (fun c -> c.HitTest p)) with
    | Some c -> c.OnMouseDown(e)
    | None -> 
      let pw = this.TransformPoint transform.V2W p
      let controlsWorld = controls |> Seq.filter (fun c -> c.CoordinateType = World)
      match (controlsWorld |> Seq.tryFind(fun c -> c.HitTest pw)) with
      | Some c -> c.OnMouseDown(e)
      | None -> ()

  override this.OnMouseMove e =
    let p = PointF(single e.X, single e.Y)
    let controlsView = controls |> Seq.filter (fun c -> c.CoordinateType = View)
    match (controlsView |> Seq.tryFind (fun c -> c.HitTest p)) with
    | Some c -> c.OnMouseMove(e)
    | None -> ()

  override this.OnMouseUp e =
    let p = PointF(single e.X, single e.Y)
    let controlsView = controls |> Seq.filter (fun c -> c.CoordinateType = View)
    match (controlsView |> Seq.tryFind (fun c -> c.HitTest p)) with
    | Some c -> c.OnMouseUp(e)
    | None -> ()

  override this.OnPaint e =
    let g = e.Graphics

    let t = g.Transform

    g.Transform <- transform.W2V

    for idx in (controls.Count - 1) .. -1 .. 0 do
      let c = controls.[idx]
      if c.CoordinateType = World then
        c.OnPaint e
    
    g.Transform <- t

    for idx in (controls.Count - 1) .. -1 .. 0 do
      let c = controls.[idx]
      if c.CoordinateType = View then
        c.OnPaint e

  override this.OnKeyDown e =
    match e.KeyCode with
    | Keys.W ->
      transform.Translate(0.f, 10.f)
      this.Invalidate()
    | Keys.A -> 
      transform.Translate(10.f, 0.f)
      this.Invalidate()
    | Keys.S -> 
      transform.Translate(0.f, -10.f)
      this.Invalidate()
    | Keys.D ->
      transform.Translate(-10.f, 0.f)
      this.Invalidate()
    | Keys.Q ->
      transform.Rotate(10.f)
      this.Invalidate()
    | Keys.E ->
      transform.Rotate(-10.f)
      this.Invalidate()
    | Keys.Z ->
      transform.Scale(1.1f, 1.1f)
      this.Invalidate()
    | Keys.X ->
      transform.Scale(1.f/1.1f, 1.f/1.1f)
      this.Invalidate()
    | _ -> ()