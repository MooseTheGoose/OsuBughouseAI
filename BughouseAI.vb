Imports System
Imports System.Windows.Forms
Imports System.Text

Namespace BughouseAI

Module BughouseEntry

Sub Main(args As String())
  Dim data = New BughouseData()
  Dim suffix = data.MakeMove("e4")
  data.MakeMove("e5")
  data.MakeMove("d3")
  Dim board = New StringBuilder()
  If Not suffix Is Nothing
    Console.WriteLine("We did it, gang!")
  End If
  For i = 0 To 7
    For j = 0 To 7
      board.Append(data.board(i, j))
    Next
    board.Append(Convert.ToChar(&H0A))
  Next
  Console.WriteLine(board)
  Console.WriteLine(data.castlingRights)
  Console.WriteLine(data.enPassant)
  Console.WriteLine("Hello, World!")
End Sub

End Module

End Namespace