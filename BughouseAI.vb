Imports System
Imports System.Text

Namespace BughouseAI
  Module BughouseEntry
    Sub PrintBoards(data As BughouseData)
      Dim board = New StringBuilder()
      Dim boardY = 5
      Dim boardX = 1
      Dim boardWidth = (2 * BughouseData.NColumns + 8) + boardX
      Dim boardHeight = (2 * BughouseData.NRows + 3)
      Dim separatorX = boardWidth - 3
      Dim boardMap = New Char((boardY + boardHeight), (boardX + boardWidth) * 2) {}
      Dim boardLabelA = "Board A"
      Dim boardLabelB = "Board B"
      Dim boardLabelAX = separatorX \ 2
      Dim boardLabelBX = separatorX + boardLabelAX

      For i = 0 To boardMap.GetLength(0) - 1
        For j = 0 To boardMap.GetLength(1) - 1
          boardMap(i, j) = " "c
        Next
      Next

      For i = 0 To boardLabelA.Length - 1
        Dim labelX = boardLabelAX - (boardLabelA.Length \ 2) + i
        boardMap(boardY - 3, labelX) = boardLabelA(i)
      Next
      For i = 0 To boardLabelA.Length - 1
        Dim labelX = boardLabelBX - (boardLabelB.Length \ 2) + i
        boardMap(boardY - 3, labelX) = boardLabelB(i)
      Next

      For i = 0 To boardMap.GetLength(0) - 1
        boardMap(i, separatorX) = "|"c
      Next

      For j = 0 To BughouseData.NColumns - 1
        boardMap(boardY - 1, boardX + 3 + 2 * j) = Convert.ToChar(Convert.ToInt32("a"c) + j)
      Next
      For i = 0 To BughouseData.NRows - 1
        boardMap(2 * i + 1 + boardY, boardX) = Convert.ToChar(Convert.ToInt32("8"c) - i)
        boardMap(2 * i + 1 + boardY, boardX + 2) = "|"c
        For j = 0 To BughouseData.NColumns - 1
          boardMap(2 * i + boardY, boardX + 2 * j + 3) = "-"c
          boardMap(2 * i + 2 + boardY, boardX + 2 * j + 3) = "-"c
          boardMap(2 * i + 1 + boardY, boardX + 2 * j + 3) = data.board(i, j)
          boardMap(2 * i + 1 + boardY, boardX + 2 * j + 4) = "|"c
        Next
      Next
      boardX += (separatorX + 1)
      For j = 0 To BughouseData.NColumns - 1
        boardMap(boardY - 1, boardX + 3 + 2 * j) = Convert.ToChar(Convert.ToInt32("a"c) + j)
      Next
      For i = 0 To BughouseData.NRows - 1
        boardMap(2 * i + 1 + boardY, boardX) = Convert.ToChar(Convert.ToInt32("8"c) - i)
        boardMap(2 * i + 1 + boardY, boardX + 2) = "|"c
        For j = 0 To BughouseData.NColumns - 1
          boardMap(2 * i + boardY, boardX + 2 * j + 3) = "-"c
          boardMap(2 * i + 2 + boardY, boardX + 2 * j + 3) = "-"c
          boardMap(2 * i + 1 + boardY, boardX + 2 * j + 3) = data.buddy.board(i, j)
          boardMap(2 * i + 1 + boardY, boardX + 2 * j + 4) = "|"c
        Next
      Next

      For i = 0 To boardMap.GetLength(0) - 1
        For j = 0 To boardMap.GetLength(1) - 1
          board.Append(boardMap(i, j))
        Next
        board.Append(Convert.ToChar(&HA))
      Next
      Console.WriteLine(board.ToString())
    End Sub

    Sub BughouseConsole()
      Dim data = New BughouseData(Nothing)
      data.buddy = New BughouseData(data)

      While True
        PrintBoards(data)
        Console.Write("What do you wanna do? ")
        Dim response = Console.ReadLine().ToLower()
        If response = "q" Or response = "quit" Or response = "exit" Or response = "e" Then
          Exit While
        ElseIf response = "p" Or response = "play" Then
          Console.Write("Which board? ")
          response = Console.ReadLine().ToLower()
          Console.Write("SAN move? ")
          Dim move = Console.ReadLine()
          Dim suffix As String = ""
          If response = "a" Then
            suffix = data.MakeMove(move)
          ElseIf response = "b" Then
            suffix = data.buddy.MakeMove(move)
          Else
            Console.WriteLine("Invalid input")
            Console.WriteLine()
            Continue While
          End If
          If suffix = BughouseData.InvalidSuffix Then
            Console.WriteLine("Move illegal or bad format")
          ElseIf suffix = "#" Then
            Console.WriteLine("Checkmate!")
          ElseIf suffix = "+" Then
            Console.WriteLine("Check!")
          Else
            Console.WriteLine("Move made")
          End If
          Console.WriteLine()
        End If
      End While
    End Sub

    Sub Main(args As String())
      BughouseConsole()
    End Sub
  End Module
End Namespace
