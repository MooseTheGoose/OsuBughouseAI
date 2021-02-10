Imports System
Imports System.Text
Imports System.Threading
Imports System.Windows.Forms


Namespace BughouseAI

Module BughouseEntry

Sub Main(args As String())
  Dim data = New BughouseData()
  Dim board = New StringBuilder()
  Dim window As MainWindow

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

  window = New MainWindow()
End Sub
End Module

End Namespace