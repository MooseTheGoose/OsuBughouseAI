Imports System
Imports System.Windows.Forms
Imports System.Threading

Namespace BughouseAI
  Class MainWindow
    Public mainWin As Form
    Public height As Integer
    Public width As Integer
    Public boardPic As Image
    Public piecesPic As Image
    Public winGraphics As Graphics
    Public textInput As TextBox
    Public submitButton As Button
    Public data As BughouseData

    Public Const StartingHeight = 500
    Public Const StartingWidth = 500
    Public ReadOnly submitDimensions = New PointF(0.2F, 0.05F)
    Public ReadOnly textboxDimensions = New PointF(0.7F, 0.05F)
    Public ReadOnly submitPosition = New PointF(0.75F, 0.85F)
    Public ReadOnly textboxPosition = New PointF(0.01F, 0.855F)
    Public ReadOnly boardDimensions = New PointF(0.95F, 0.8F)
    Public ReadOnly boardPosition = New PointF(0.01F, 0.01F)
    Public ReadOnly piecesOrder = "kqrnbp"
    Public ReadOnly whitePositionY = 0.5F
    Public ReadOnly blackPositionY = 0.0F

    Public Sub OnMainPaint(sender As Object, e As EventArgs)
      Dim x = boardPosition.X * width
      Dim y = boardPosition.Y * height
      Dim w = boardDimensions.X * width
      Dim h = boardDimensions.Y * height
      winGraphics.DrawImage(boardPic, New Rectangle(x, y, w, h))

      For i = 0 To BughouseData.NRows - 1
        For j = 0 To BughouseData.NColumns - 1
          If data.board(i, j) <> " "c Then
            Dim index = piecesOrder.IndexOf(Char.ToLower(data.board(i, j)))
            Dim px = (index / 6) * piecesPic.Width
            Dim py = If(Char.IsLower(data.board(i, j)), blackPositionY, whitePositionY) * piecesPic.Height
            Dim pw = piecesPic.Width / 6
            Dim ph = piecesPic.Height / 2
            x = ((boardPosition.X + boardDimensions.X) * width * j) / BughouseData.NColumns
            y = ((boardPosition.Y + boardDimensions.Y) * height * i) / BughouseData.NRows
            w = boardDimensions.X * width / BughouseData.NColumns
            h = boardDimensions.Y * height / BughouseData.NRows
            winGraphics.DrawImage(piecesPic, New Rectangle(x, y, w, h), New Rectangle(px, py, pw, ph), GraphicsUnit.Pixel)
          End If
        Next
      Next
    End Sub

    Public Sub EnterKeyPress(sender As Object, e As KeyPressEventArgs)
      Dim textBox As TextBox = sender
      If e.KeyChar = Convert.ToChar(&HD) Then
        Dim move = textBox.Text.Trim()
        Dim suffix = data.MakeMove(move)
        If suffix <> BughouseData.InvalidSuffix Then
          textBox.Text = ""
        End If
        OnMainPaint(Nothing, Nothing)
      End If
    End Sub


    Public Sub New()
      mainWin = New Form()
      height = StartingHeight
      width = StartingWidth
      mainWin.Size = New Point(width, height)
      mainWin.FormBorderStyle = FormBorderStyle.FixedSingle
      boardPic = Image.FromFile("Chess_Board.png")
      piecesPic = Image.FromFile("Chess_Pieces.png")
      winGraphics = mainWin.CreateGraphics()
      textInput = New TextBox()
      textInput.Location = New Point(textboxPosition.X * width, textboxPosition.Y * height)
      textInput.Size = New Point(textboxDimensions.X * width, textboxDimensions.Y * height)
      submitButton = New Button()
      submitButton.Text = "Submit!"
      submitButton.Location = New Point(submitPosition.X * width, submitPosition.Y * height)
      submitButton.Size = New Point(submitDimensions.X * width, submitDimensions.Y * height)
      mainWin.Controls.Add(textInput)
      mainWin.Controls.Add(submitButton)
      data = New BughouseData()
      data.buddy = New BughouseData(data)
      AddHandler mainWin.Paint, AddressOf OnMainPaint
      AddHandler textInput.KeyPress, AddressOf EnterKeyPress
      mainWin.ShowDialog()
    End Sub
  End Class
End Namespace
