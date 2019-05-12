open System.Web.Services.Description
open System
#load "lwc.fsx"

open System.Windows.Forms
open System.Drawing

open Lwc

let mutable animated = false

type PIImage(fileName: string) as this =
    inherit LWControl()

    //immagine aperta
    //let imm = Image.FromFile(fileName)
    let imm = new Bitmap(fileName)
    let min = 30.f
    let max = 400.f
    let def = 150.f
    let mutable selected = false
    let mutable angle = 0.f
    let mutable lastPointDraw :(PointF)option = None

    do
        this.CoordinateType <- World
        if imm.Width = imm.Height then this.Size <- SizeF(def, def)
        elif imm.Width > imm.Height then
            let h = def * (single)imm.Height / (single)imm.Width
            this.Size <- SizeF(def, h)
        else 
            let w = def * (single)imm.Width / (single)imm.Height
            this.Size <- SizeF(w, def)

    member this.Selected with get() = selected
                          and set(a: bool) = selected <- a

    member this.Angle with get() = angle

    member this.Center with get() = PointF(this.Position.X + (this.Size.Width/2.f), this.Position.Y + (this.Size.Height / 2.f))

    member this.UpdateSize (v :float32) = 
        let newWidth = this.Size.Width * v
        let newHeight = this.Size.Height * v
        if (newHeight >= min && newHeight <= max && newWidth >= min && newWidth <= max) then
            let newX = this.Position.X + (this.Size.Width - newWidth) / 2.f
            let newY = this.Position.Y + (this.Size.Height - newHeight) / 2.f
            this.Size <- SizeF(newWidth, newHeight)
            this.Position <- PointF(newX, newY)

    member this.UpdateWidth (v :float32) = 
        let newWidth = this.Size.Width * v
        if (newWidth >= min && newWidth <= max) then
            let newX = this.Position.X + (this.Size.Width - newWidth) / 2.f
            this.Size <- SizeF(newWidth, this.Size.Height)
            this.Position <- PointF(newX, this.Position.Y)

    member this.UpdateHeight (v :float32) =
        let newHeight = this.Size.Height * v
        if (newHeight >= min && newHeight <= max) then
            let newY = this.Position.Y + (this.Size.Height - newHeight) / 2.f
            this.Size <- SizeF(this.Size.Width, newHeight)
            this.Position <- PointF(this.Position.X, newY)

    member this.UpdateAngle (a :float32) = 
        angle <- (angle + a)

    member this.UpdatePosition (upX :float32, upY :float32) = 
        this.Position <- PointF(this.Position.X + upX, this.Position.Y + upY)

    member this.Dispose _ =
        imm.Dispose()

    member this.HandleRect (p:PointF) (r:single) =
        let d = 2.f * r
        RectangleF(p.X - r, p.Y - r, d, d)

    member this.PointRespectImm (pv: PointF, mtx: W2V) =
        let save = W2V()
        save.W2V <- mtx.W2V.Clone()
        save.V2W <- mtx.V2W.Clone()
        save.Translate(this.Center.X,this.Center.Y)
        save.Rotate(angle)
        save.Translate(-this.Center.X,-this.Center.Y)
        let pts = [| pv |]
        save.V2W.TransformPoints(pts)
        pts.[0]
        
    member this.HitImage (pv: PointF, mtx: W2V) = 
        let pw = this.PointRespectImm(pv,mtx)
        this.HitTest pw

    member this.AddDrawPoint (pv : PointF, mtx: W2V) = 
        let mutable pw = this.PointRespectImm(pv,mtx)
        pw <- PointF(pw.X-this.Position.X,pw.Y-this.Position.Y)
        pw <- PointF(pw.X/this.Size.Width*(float32)imm.Width,pw.Y/this.Size.Height*(float32)imm.Height)

        let g = Graphics.FromImage(imm)
        let r = 4.f
        g.FillEllipse(Brushes.Red,RectangleF(PointF(pw.X-r,pw.Y-r),SizeF(2.f*r,2.f*r)))
        match lastPointDraw with
        | Some(p) -> 
            g.DrawLine(Pens.Red, p, pw)
            lastPointDraw <- Some(pw)
        | _ -> ()

    override this.OnPaint e =
        let g = e.Graphics
        let sz = this.Size
        let pw = this.Position
        let centerW = PointF(pw.X + (sz.Width / 2.f), pw.Y + (sz.Height / 2.f))
        
        let t = g.Transform

        g.TranslateTransform(centerW.X, centerW.Y)
        g.RotateTransform(angle)

        let boxRect = RectangleF(-(sz.Width / 2.f), -(sz.Height / 2.f), sz.Width, sz.Height)
        g.DrawImage(imm, boxRect)
        if selected then g.DrawRectangle(Pens.Red, -(sz.Width / 2.f), -(sz.Height / 2.f), sz.Width, sz.Height)
            
        g.Transform <- t
        


type Action =
    | SelImm = 0
    | SelPila = 1
    | DrawImm = 2
    | No = 3


type Box() as this =
    inherit LWContainer()

    let mutable images = ResizeArray<PIImage>()
    let mutable imSelected = ResizeArray<PIImage>()

    let mutable drag: (PIImage * PointF)option = None //immagine che sto spostando e delta

    //informazioni mentre disegno: immagine * ultimo punto disegnato
    let mutable draw: (PIImage)option = None 
    let mutable pen = new Pen(Color.Red)

    let mutable action: Action = Action.No //default

    let mutable animation = ResizeArray<(float32 * float32 * float32)>() // x * y
    let mutable tickN = 0

    let timer = new Timer(Interval=1000/60)     

    do
        pen.Width <- 6.f
        timer.Start()//avvio il timer
        timer.Tick.Add(fun _ ->
            if tickN > 0 then
                for i in 1..(imSelected.Count - 1) do
                    let imm = imSelected.[i]
                    let update = animation.[i-1]
                    match update with 
                        | (upX, upY, a) ->
                            imm.UpdatePosition(upX,upY) |> ignore
                            imm.UpdateAngle(a) 
                tickN <- tickN - 1
                if tickN = 0 then animated <- false
            this.Invalidate()
        )

    member this.ResetSelected _ =
        for i in 0..(imSelected.Count - 1) do
            imSelected.[i].Selected <- false
        imSelected <- new ResizeArray<PIImage>()
    
    member this.Azione with get() = action
                        and set(a) = action <- a

    member this.NewImm (fileName: string) = 
        let imm = PIImage(fileName);
        imm.Parent <- this
        let centerV = PointF((float32)(this.Size.Width/2), (float32)(this.Size.Height/2))
        let centerW = this.TransformPoint this.Transform.V2W centerV
        imm.Position <- PointF(centerW.X - imm.Size.Width/2.f , centerW.Y - imm.Size.Height/2.f)
        images.Add(imm)
        this.LWControls.Add(imm)
        this.Invalidate()

    member this.DeleteImm _ = 
        for i in 0..(imSelected.Count - 1) do
            let imm = imSelected.[i]
            images.RemoveAll(fun n1 -> n1 = imm) |> ignore
            this.LWControls.RemoveAll(fun n1 -> n1 = upcast imm) |> ignore
            imm.Dispose()
        imSelected <- new ResizeArray<PIImage>()
        action <- Action.No
        this.Invalidate()

    member this.ImpilaSel _ = 
        if imSelected.Count > 1 then 
            let tickAnimation = 100.f
            animation <- new ResizeArray<(float32 * float32 * float32)>()
            //prendo le informazioni del primo elemento "base della pila"
            let baseCtr = imSelected.[0].Center
            let baseAngle = imSelected.[0].Angle
            //cotruisco l'array per le informazioni sulle animazioni
            for i in 1..(imSelected.Count - 1) do
                let idxCtr = imSelected.[i].Center
                let idxAngle = imSelected.[i].Angle
                let tickX = (baseCtr.X - idxCtr.X) / tickAnimation
                let tickY = (baseCtr.Y - idxCtr.Y) / tickAnimation
                let tickAngle = (baseAngle - idxAngle)  / tickAnimation
                animation.Add(tickX,tickY,tickAngle)
            tickN <- (int)tickAnimation
            animated <- true
    
    member this.Up _ = (
        this.Transform.Translate(0.f, 10.f)
        this.Invalidate()
    )
    member this.Down _ = (
        this.Transform.Translate(0.f, -10.f)
        this.Invalidate()
    )
    member this.Left _ = (
        this.Transform.Translate(10.f, 0.f)
        this.Invalidate()
    )
    member this.Right _ = (
        this.Transform.Translate(-10.f, 0.f)
        this.Invalidate()
    )
    member this.RotateL _ = (
        this.Transform.Rotate(10.f)
        this.Invalidate()
    )
    member this.RotateR _ = (
        this.Transform.Rotate(-10.f)
        this.Invalidate()
    )
    member this.ZoomIn _ = (
        this.Transform.Scale(1.1f, 1.1f)
        this.Invalidate()
    )
    member this.ZoomOut _ = (
        this.Transform.Scale(1.f/1.1f, 1.f/1.1f)
        this.Invalidate()
    )
    
    member this.ExpandImmSel _ = (
        for i in 0..(imSelected.Count - 1) do
            imSelected.[i].UpdateSize (1.1f)
        this.Invalidate()
    )
    member this.DwindleImmSel _ = (
        for i in 0..(imSelected.Count - 1) do
            imSelected.[i].UpdateSize (1.f/1.1f)
        this.Invalidate()
    )
    member this.RotRImmSel _ = (
        for i in 0..(imSelected.Count - 1) do
            imSelected.[i].UpdateAngle(-10.f)
        this.Invalidate()
    )
    member this.RotLImmSel _ = (
        for i in 0..(imSelected.Count - 1) do
            imSelected.[i].UpdateAngle(10.f)
        this.Invalidate()
    )
    member this.StretchH _ = (
        for i in 0..(imSelected.Count - 1) do
            imSelected.[i].UpdateHeight (1.1f)
        this.Invalidate()
    )
    member this.StretchW _ = (
        for i in 0..(imSelected.Count - 1) do
            imSelected.[i].UpdateWidth (1.1f)
        this.Invalidate()
    )
    member this.ShrinkH _ = (
        for i in 0..(imSelected.Count - 1) do
            imSelected.[i].UpdateHeight (1.f/1.1f)
        this.Invalidate()
    )
    member this.ShrinkW _ = (
        for i in 0..(imSelected.Count - 1) do
            imSelected.[i].UpdateWidth (1.f/1.1f)
        this.Invalidate()
    )

    override this.OnMouseDown e =
        base.OnMouseDown e
        let p = PointF(single e.X, single e.Y)
        if not animated then
            //controllo se è stato cliccato un bottone per modificare la vista
            let controlsView = this.LWControls |> Seq.filter (fun c -> c.CoordinateType = View)
            match (controlsView |> Seq.tryFind (fun c -> c.HitTest p)) with
            | Some c -> ()
            | None ->
                //controllo se è stata cliccata una immagine
                match (images |> Seq.tryFind(fun c -> c.HitImage(p,this.Transform) )) with
                | Some c ->
                    match action with 
                    | Action.SelImm -> 
                        match c.Selected with
                        | false -> 
                            c.Selected <- true
                            imSelected.Add(c)
                        | _ -> ()
                    | Action.SelPila ->
                        for i in 0..(images.Count - 1) do
                            let imm = images.[i]
                            if (PointsQuiteEqual(c.Center,imm.Center, 0.1f)) then
                                match imm.Selected with
                                | false -> 
                                    imm.Selected <- true
                                    imSelected.Add(imm)
                                | _ -> ()
                    | Action.DrawImm ->
                        c.AddDrawPoint(p,this.Transform)
                        draw <- Some(c)
                        this.Invalidate()
                    | _ ->
                        //drag disponibile solo se non sono in fasi particolari
                        let pv = this.TransformPoint this.Transform.W2V c.Position
                        drag <- Some(c, PointF(pv.X - p.X, pv.Y - p.Y))
                | None -> 
                    match action with
                    | Action.SelImm -> 
                        this.ResetSelected()
                        action <- Action.No
                    | Action.SelPila -> 
                        this.ResetSelected()
                        action <- Action.No
                    | _ -> ()


    override this.OnMouseUp e =
        base.OnMouseUp e
        match action with
        | Action.DrawImm ->
            draw <- None
        | _ -> drag <- None


    override this.OnMouseMove e =
        base.OnMouseMove e
        match action with
        | Action.DrawImm ->
            match draw with
            | Some(c) ->
                let p = PointF(single e.X, single e.Y)
                if c.HitImage(p,this.Transform) then
                    c.AddDrawPoint(p,this.Transform)
                    this.Invalidate()
                else
                    draw <- None
            | None -> ()
        | _ ->
            match drag with
            | Some (imm, d) ->
                let pa = PointF(float32 e.X + d.X, float32 e.Y + d.Y)
                let pw = this.TransformPoint this.Transform.V2W pa
                imm.Position <- pw
                this.Invalidate()
            | None -> ()

    override this.OnKeyDown e =
        base.OnKeyDown e


//LWControl per i bottoni per il controllo vista
type PIButton() =
    inherit LWControl()

    let mutable text = ""

    member this.Text
        with get() = text
        and set(v) = text <- v

    override this.OnPaint e =
        let parent = this.Parent
        let g = e.Graphics

        let r = RectangleF(this.Position, this.Size) |> RectF2Rect
        g.FillRectangle(Brushes.White,r)
        g.DrawRectangle(Pens.Black, r)

        let ssz = g.MeasureString(text, parent.Font)
        let p = this.Position
        let sz = this.Size
        let sx, sy = p.X + (sz.Width - ssz.Width) / 2.f, p.Y + (sz.Height - ssz.Height) / 2.f
        g.DrawString(text, parent.Font, Brushes.Black, PointF(sx, sy))


///////////////////////////////////////////////////////////////////////////
//creo il contenitore
let c = new Box(Dock=DockStyle.Fill)

//bottoni per il controllo vista
let zoom = PIButton(Position=PointF(30.f, 0.f), Size=SizeF(30.f, 30.f), Text="+")
zoom.Parent <- c
zoom.MouseDown.Add(fun _ ->
    if not animated then c.ZoomIn())

let dezoom = PIButton(Position=PointF(60.f, 0.f), Size=SizeF(30.f, 30.f), Text="-")
dezoom.Parent <- c
dezoom.MouseDown.Add(fun _ ->
    if not animated then c.ZoomOut())

let ruotaR = PIButton(Position=PointF(90.f, 0.f), Size=SizeF(30.f, 30.f), Text="L")
ruotaR.Parent <- c
ruotaR.MouseDown.Add(fun _ ->
    if not animated then c.RotateR())

let ruotaL = PIButton(Position=PointF(120.f, 0.f), Size=SizeF(30.f, 30.f), Text="R")
ruotaL.Parent <- c
ruotaL.MouseDown.Add(fun _ ->
    if not animated then c.RotateL())

let su = PIButton(Position=PointF(150.f, 0.f), Size=SizeF(30.f, 30.f), Text="▲")
su.Parent <- c
su.MouseDown.Add(fun _ ->
    if not animated then c.Up())

let giu = PIButton(Position=PointF(180.f, 0.f), Size=SizeF(30.f, 30.f), Text="▼")
giu.Parent <- c
giu.MouseDown.Add(fun _ ->
    if not animated then c.Down())

let sx = PIButton(Position=PointF(210.f, 0.f), Size=SizeF(30.f, 30.f), Text="◄")
sx.Parent <- c
sx.MouseDown.Add(fun _ ->
    if not animated then c.Left())

let dx = PIButton(Position=PointF(240.f, 0.f), Size=SizeF(30.f, 30.f), Text="►")
dx.Parent <- c
dx.MouseDown.Add(fun _ ->
    if not animated then c.Right())


//bottoni per gestire immagini
let newImm = PIButton(Position = PointF(0.f, 60.f), Size=SizeF(50.f,30.f), Text="New")
newImm.Parent <- c
newImm.MouseDown.Add(fun _ ->
    if not animated then 
        c.ResetSelected()
        let dlg = new OpenFileDialog()
        dlg.Title <- "Open Image"
        dlg.Filter <- "images| *.JPG; *.PNG; *.GJF"
        if (dlg.ShowDialog() = DialogResult.OK) then
            let txt = dlg.FileName
            dlg.Dispose()
            c.NewImm(txt)
)

let remove = PIButton(Position = PointF(50.f, 60.f), Size=SizeF(50.f,30.f), Text="Rem")
remove.Parent <- c
remove.MouseDown.Add(fun _ ->
    if not animated then c.DeleteImm())

let selImm = PIButton(Position = PointF(0.f, 90.f), Size=SizeF(50.f,30.f), Text="Sel Imm")
selImm.Parent <- c
selImm.MouseDown.Add(fun _ ->
    if not animated then c.Azione <- Action.SelImm)

let selPila = PIButton(Position=PointF(50.f, 90.f), Size=SizeF(50.f, 30.f), Text="Sel Pila")
selPila.Parent <- c
selPila.MouseDown.Add(fun _ ->
    if not animated then c.Azione <- Action.SelPila)

let actPila = PIButton(Position=PointF(0.f, 120.f), Size=SizeF(50.f, 30.f), Text="Impila")
actPila.Parent <- c
actPila.MouseDown.Add(fun _ ->
    if not animated then c.ImpilaSel())

let drawImm = PIButton(Position=PointF(50.f, 120.f), Size=SizeF(50.f, 30.f), Text="Draw")
drawImm.Parent <- c
drawImm.MouseDown.Add(fun _ ->
    if not animated then 
        c.ResetSelected()
        c.Azione <- Action.DrawImm
)

//bottoni per manipolare immagini
let zoomImm = PIButton(Position=PointF(0.f, 180.f), Size=SizeF(50.f, 30.f), Text="+ Imm")
zoomImm.Parent <- c
zoomImm.MouseDown.Add(fun _ ->
    if not animated then c.ExpandImmSel())

let dezoomImm = PIButton(Position=PointF(50.f, 180.f), Size=SizeF(50.f, 30.f), Text="- Imm")
dezoomImm.Parent <- c
dezoomImm.MouseDown.Add(fun _ ->
    if not animated then c.DwindleImmSel())

let ruotaRImm = PIButton(Position=PointF(0.f, 210.f), Size=SizeF(50.f, 30.f), Text="R Imm")
ruotaRImm.Parent <- c
ruotaRImm.MouseDown.Add(fun _ ->
    if not animated then c.RotRImmSel())

let ruotaLImm = PIButton(Position=PointF(50.f, 210.f), Size=SizeF(50.f, 30.f), Text="L Imm")
ruotaLImm.Parent <- c
ruotaLImm.MouseDown.Add(fun _ ->
    if not animated then c.RotLImmSel())

let stretchH = PIButton(Position=PointF(0.f, 240.f), Size=SizeF(50.f, 30.f), Text="stretch H")
stretchH.Parent <- c
stretchH.MouseDown.Add(fun _ ->
    if not animated then c.StretchH())

let stretchW = PIButton(Position=PointF(50.f, 240.f), Size=SizeF(50.f, 30.f), Text="stretch W")
stretchW.Parent <- c
stretchW.MouseDown.Add(fun _ ->
    if not animated then c.StretchW())

let shrinkH = PIButton(Position=PointF(0.f, 270.f), Size=SizeF(50.f, 30.f), Text="shrink H")
shrinkH.Parent <- c
shrinkH.MouseDown.Add(fun _ ->
    if not animated then c.ShrinkH())

let shrinkW = PIButton(Position=PointF(50.f, 270.f), Size=SizeF(50.f, 30.f), Text="shrink W")
shrinkW.Parent <- c
shrinkW.MouseDown.Add(fun _ ->
    if not animated then c.ShrinkW())


c.LWControls.Add(zoom)
c.LWControls.Add(dezoom)
c.LWControls.Add(ruotaR)
c.LWControls.Add(ruotaL)
c.LWControls.Add(su)
c.LWControls.Add(giu)
c.LWControls.Add(dx)
c.LWControls.Add(sx)

c.LWControls.Add(newImm)
c.LWControls.Add(remove)
c.LWControls.Add(selImm)
c.LWControls.Add(selPila)
c.LWControls.Add(actPila)
c.LWControls.Add(drawImm)

c.LWControls.Add(zoomImm)
c.LWControls.Add(dezoomImm)
c.LWControls.Add(ruotaRImm)
c.LWControls.Add(ruotaLImm)
c.LWControls.Add(stretchH)
c.LWControls.Add(stretchW)
c.LWControls.Add(shrinkH)
c.LWControls.Add(shrinkW)

let f = new Form()
f.Size <- Size(700,700)
f.Controls.Add(c)
f.Show()
