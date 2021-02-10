Imports System
Imports System.Collections.Generic

Namespace BughouseAI

Class BughouseData
  Public board As Char(,)
  Public enPassant As String
  Public color As Char
  Public castlingRights As String
  Public reserve As List(Of Char)
  Public promotedSquares As List(Of String)
  Public buddy As BughouseData
  Public myTime As Integer
  Public buddyTime As Integer
  Public myOppTime As Integer
  Public buddyOppTime As Integer

  ' Do not change the case of PieceNames and BackRank
  Public Shared ReadOnly PieceNames = "rnbkqp"
  Public Shared ReadOnly BackRank = "rnbqkbnr"
  Public Shared ReadOnly SLComment = ";"
  Public Shared ReadOnly OpenComment = "{"
  Public Shared ReadOnly CloseComment = "}"
  Public Const NRows = 8
  Public Const NColumns = 8

  Public Sub New()
    Me.board = New Char(NRows - 1, NColumns - 1) {}
    Me.enPassant = "-"
    Me.color = "w"
    Me.castlingRights = "KQkq"

    For index = 0 To BackRank.Length - 1
      Me.board(0, index) = BackRank(index)
      Me.board(1, index) = "p"c
      Me.board(2, index) = " "c
      Me.board(3, index) = " "c
      Me.board(4, index) = " "c
      Me.board(5, index) = " "c
      Me.board(6, index) = "P"c
      Me.board(7, index) = Char.ToUpper(BackRank(index))
    Next
    Me.buddy = Nothing
    Me.reserve = New List(Of Char)()
    Me.promotedSquares = New List(Of String)()
    Me.myTime = 0
    Me.myOppTime = 0
    Me.buddyTime = 0
    Me.buddyOppTime = 0
  End Sub

  Public Sub New(buddy As BughouseData)
    Me.buddy = buddy
    buddy.buddy = Me

    Me.board = New Char(NRows - 1, NColumns - 1) {}
    Me.enPassant = "-"
    Me.color = "w"
    Me.castlingRights = "KQkq"

    For index = 0 To BackRank.Length - 1
      Me.board(0, index) = BackRank(index)
      Me.board(1, index) = "p"c
      Me.board(2, index) = " "c
      Me.board(3, index) = " "c
      Me.board(4, index) = " "c
      Me.board(5, index) = " "c
      Me.board(6, index) = "P"c
      Me.board(7, index) = Char.ToUpper(BackRank(index))
    Next
  End Sub

#If False
  ' Return true iff parse was successful.
  ' If failed, don't change the underlying structure.
  '
  ' ... Now that I think about it, FEN isn't sufficient to
  ' restart a BugHouse game. By a long shot. Shoot.
  ' Luckily, there's BFEN
  Public Function ParseFEN(fen As String)
    Dim tempData = New BughouseData()
    Dim cursorX = 0
    Dim cursorY = 0
    Dim index = 0

    While index < fen.Length AndAlso Char.IsWhiteSpace(fen(index))
      index += 1
    End While

    ' Set up board
    While index < fen.Length And cursorY < NRows
      If PieceNames.Contains(Char.ToLower(fen(index)))
        If cursorX >= NColumns
          Return False
        End If
        tempData.board(cursorY, cursorX) = fen(index)
        cursorX += 1
        index += 1
        Continue While
      End If

      If Char.IsDigit(fen(index)) And fen(index) <> "0"c
        For i = 0 To Val(fen(index)) - 1
          If cursorX >= NColumns
            Return False
          End If
          tempData.board(cursorY, cursorX) = " "c
          cursorX += 1
        Next
        index += 1
        Continue While
      End If

      If fen(index) = "/"c
        cursorY += 1
        If cursorX <> NColumns Or cursorY >= NRows
         Return False
        End If
        cursorX = 0
        index += 1
        Continue While
      End If

      If Char.IsWhiteSpace(fen(index))
        cursorY += 1
        If cursorY <> NRows
          Return False
        End If
        index += 1
        Continue While
      End If

      Return False
    End While

    While index < fen.Length AndAlso Char.IsWhiteSpace(fen(index))
      index += 1
    End While

    ' Determine color
    If index >= fen.Length OrElse (fen(index) <> "w"c And fen(index) <> "b"c)
      Return False
    End If
    tempData.color = fen(index)
    index += 1

    If index >= fen.Length OrElse Not Char.IsWhiteSpace(fen(index))
      Return False
    End If
    While index < fen.Length AndAlso Char.IsWhiteSpace(fen(index))
      index += 1
    End While

    ' Determine castling rights
    If index >= fen.Length
      Return False
    End If
    If fen(index) = "-"c
      tempData.castlingRights = "-"
      index += 1
    Else
      tempData.castlingRights = ""
      While index < fen.Length AndAlso (Char.ToLower(fen(index)) = "k"c Or Char.ToLower(fen(index)) = "q"c)
        If tempData.castlingRights.Contains(fen(index))
          Return False
        End If
        tempData.castlingRights += fen(index)
        index += 1
      End While
    End If

    If index >= fen.Length OrElse Not Char.IsWhiteSpace(fen(index))
      Return False
    End If
    While index < fen.Length AndAlso Char.IsWhiteSpace(fen(index))
      index += 1
    End While

    ' Determine en passant square
    If index >= fen.Length
      Return False
    End If
    If fen(index) = "-"c
      tempData.enPassant = "-"
      index += 1
    Else
      If index + 1 >= fen.Length _
         OrElse (fen(index) >= "a"c And fen(index) <= "h"c) _
         OrElse (fen(index + 1) >= "1"c And fen(index + 1) <= "8"c)
        tempData.enPassant = fen.SubString(index, 2)
        index += 2
      End If
    End If

    If Not Char.IsWhiteSpace(fen(index))
      Return False
    End If
    While index < fen.Length AndAlso Char.IsWhiteSpace(fen(index))
      index += 1
    End While

    ' Determine halfmove count
    If index >= fen.Length OrElse Not Char.IsDigit(fen(index))
      Return False
    End If
    If fen(index) = "0"c
      tempData.halfmove = 0
      index += 1
    Else
      tempData.halfmove = Val(fen(index))
      index += 1
      While index < fen.Length AndAlso Char.IsDigit(fen(index))
        If (Integer.MaxValue - Val(fen(index))) / 10 < tempData.halfmove
          Return False
        End If
        tempData.halfmove = tempData.halfmove * 10 + Val(fen(index))
        index += 1
      End While
    End If

    If Not Char.IsWhiteSpace(fen(index))
      Return False
    End If
    While index < fen.Length AndAlso Char.IsWhiteSpace(fen(index))
      index += 1
    End While

    ' Determine fullmove count
    If index >= fen.Length OrElse Not Char.IsDigit(fen(index))
      Return False
    End If
    If fen(index) = "0"c
      tempData.fullmove = 0
      index += 1
    Else
      tempData.fullmove = Val(fen(index))
      index += 1
      While index < fen.Length AndAlso Char.IsDigit(fen(index))
        If (Integer.MaxValue - Val(fen(index))) / 10 < tempData.fullmove
          Return False
        End If
        tempData.fullmove = tempData.fullmove * 10 + Val(fen(index))
        index += 1
      End While
    End If

    While index < fen.Length AndAlso Char.IsWhiteSpace(fen(index))
      index += 1
    End While
    If index <> fen.Length
      Return False
    End If

    ' Note that before you commit, you actually
    ' have to check that the opposite color isn't
    ' being checked. Do this later... 

    Me.board = tempData.board
    Me.enPassant = tempData.enPassant
    Me.enPassant = tempData.enPassant
    Me.color = tempData.color
    Me.castlingRights = tempData.castlingRights
    Me.fullmove = tempData.fullmove
    Me.halfmove = tempData.halfmove
    Return True
  End Function
#End If

  ' Return Nothing iff move can't legally made.
  ' Otherwise, return the SAN suffix ("#", "+"), or SLComment for no suffix.
  ' This is in SAN notation minus check(mate) suffix.
  ' If not, don't change underlying struct.
  Public Function MakeMove(sanNoSuffix As String)
    Dim piece = Convert.ToChar(0)
    Dim disambiguation As String = ""
    Dim srcSquare As String = Nothing
    Dim destSquare As String = Nothing
    Dim promotionPiece = Convert.ToChar(0)
    Dim saveCapture = " "c
    Dim pieceDropped = False
    Dim pieceCaptured = False

    Dim index = 0 
    While index < sanNoSuffix.Length AndAlso Char.IsWhiteSpace(sanNoSuffix(index))
      index += 1
    End While

    ' Try to get castling done right off the bat.
    Select Case sanNoSuffix
      Case "O-O"
        Return ""
      Case "O-O-O"
        Return ""
    End Select

    ' First, get the piece specified, if it is specified
    ' Note that 'P' is normally not specified, but in Bughouse,
    ' 'P' is used to denote a pawn drop (i.e. P@e5)
    If index < sanNoSuffix.Length And PieceNames.ToUpper.Contains(sanNoSuffix(index))
      piece = sanNoSuffix(index)
      index += 1
    End If

    ' Get the disambiguation, if it exists.
    ' Note that the disambiguation may be mistaken
    ' for the destination square. This will be fixed up
    ' later.
    If index < sanNoSuffix.Length AndAlso (sanNoSuffix(index) >= "a"c And sanNoSuffix(index) <= "h")
      disambiguation += sanNoSuffix(index)
      index += 1
    End If
    If index < sanNoSuffix.Length AndAlso (sanNoSuffix(index) >= "1"c And sanNoSuffix(index) <= "8"c)
      disambiguation += sanNoSuffix(index)
      index += 1
    End If

    ' Get whether the piece was dropped or not
    If index < sanNoSuffix.Length AndAlso sanNoSuffix(index) = "@"c
      pieceDropped = True
      index += 1
    End If

    ' Get whether a piece was captured or not.
    If index < sanNoSuffix.Length AndAlso sanNoSuffix(index) = "x"c
      pieceCaptured = True
      index += 1
    End If

    ' Get the destination square, or fix up disambiguation
    If index + 1 < sanNoSuffix.Length AndAlso (sanNoSuffix(index) >= "a"c And sanNoSuffix(index) <= "h"c _
         And sanNoSuffix(index + 1) >= "1"c And sanNoSuffix(index + 1) <= "8"c)
      destSquare = sanNoSuffix.SubString(index, 2)
      index += 2
    Else
      If disambiguation.Length <> 2
        Return Nothing
      End If
      destSquare = disambiguation
      disambiguation = ""
    End If


    ' Get whether a pawn was promoted
    If index + 1 < sanNoSuffix.Length AndAlso sanNoSuffix(index) = "="c
      If Not PieceNames.ToUpper.Contains(sanNoSuffix(index + 1)) _
           Or sanNoSuffix(index + 1) = "P"c Or sanNoSuffix(index + 1) = "K"c
        Return Nothing
      End If
      promotionPiece = sanNoSuffix(index + 1)
      index += 2
    End If

    While index < sanNoSuffix.Length AndAlso Char.IsWhiteSpace(sanNoSuffix(index))
      index += 1
    End While
    If index <> sanNoSuffix.Length
      Return Nothing
    End If    

    Dim dstx = Convert.ToInt32(destSquare(0)) - Convert.ToInt32("a"c)
    Dim dsty = 8 - Val(destSquare(1))
    Dim srcx As Integer, srcy As Integer
    Dim trySrc As String
    Dim pawnSgn As Integer = If(color = "w"c, 1, -1)
    If pieceDropped
      ' A whole other can of worms you shouldn't worry about for now.
      If piece = Convert.ToChar(0) Or pieceCaptured
        Return Nothing
      End If
    Else
      If piece = "P"c
        Return Nothing
      End If
      If piece = Convert.ToChar(0)
        piece = "P"c
      End If
      saveCapture = board(dsty, dstx)
      If pieceCaptured
        If (Char.IsUpper(saveCapture) And color = "w"c) Or (Char.IsLower(saveCapture) And color = "b"c) _
              Or saveCapture = " "c
          Return Nothing
        End If
      Else
        If saveCapture <> " "c
          Return Nothing
        End If
      End If

      ' Lots of inlining to keep logic simple
      ' Note that there are some cases I optimize
      ' when I know it's impossible to have ambiguities 
      ' (king, pawn w/o capture)
      Select Case piece
        Case "K"c
          srcx = dstx + 1
          srcy = dsty + 1
          If (srcx >= 0 And srcx < 8) And (srcy >= 0 And srcy < 8) _
              And ((board(srcy, srcx) = "K"c And color = "w"c) _
                 Or (board(srcy, srcx) = "k"c And color = "b"c)) 
            srcSquare = Convert.ToChar(Convert.ToInt32("a"c) + srcx) + Convert.ToString(8 - srcy)
          End If
          srcx = dstx
          srcy = dsty + 1
          If (srcx >= 0 And srcx < 8) And (srcy >= 0 And srcy < 8) _
              And ((board(srcy, srcx) = "K"c And color = "w"c) _
                 Or (board(srcy, srcx) = "k"c And color = "b"c)) 
            srcSquare = Convert.ToChar(Convert.ToInt32("a"c) + srcx) + Convert.ToString(8 - srcy)
          End If
          srcx = dstx - 1
          srcy = dsty + 1
          If (srcx >= 0 And srcx < 8) And (srcy >= 0 And srcy < 8) _
              And ((board(srcy, srcx) = "K"c And color = "w"c) _
                 Or (board(srcy, srcx) = "k"c And color = "b"c))
            srcSquare = Convert.ToChar(Convert.ToInt32("a"c) + srcx) + Convert.ToString(8 - srcy)
          End If
          srcx = dstx + 1
          srcy = dsty
          If (srcx >= 0 And srcx < 8) And (srcy >= 0 And srcy < 8) _
              And ((board(srcy, srcx) = "K"c And color = "w"c) _
                 Or (board(srcy, srcx) = "k"c And color = "b"c)) 
            srcSquare = Convert.ToChar(Convert.ToInt32("a"c) + srcx) + Convert.ToString(8 - srcy)
          End If
          srcx = dstx - 1
          srcy = dsty
          If (srcx >= 0 And srcx < 8) And (srcy >= 0 And srcy < 8) _
              And ((board(srcy, srcx) = "K"c And color = "w"c) _
                 Or (board(srcy, srcx) = "k"c And color = "b"c)) 
            srcSquare = Convert.ToChar(Convert.ToInt32("a"c) + srcx) + Convert.ToString(8 - srcy)
          End If
          srcx = dstx + 1
          srcy = dsty - 1
          If (srcx >= 0 And srcx < 8) And (srcy >= 0 And srcy < 8)  _
              And ((board(srcy, srcx) = "K"c And color = "w"c) _
                 Or (board(srcy, srcx) = "k"c And color = "b"c)) 
            srcSquare = Convert.ToChar(Convert.ToInt32("a"c) + srcx) + Convert.ToString(8 - srcy)
          End If
          srcx = dstx
          srcy = dsty - 1
          If (srcx >= 0 And srcx < 8) And (srcy >= 0 And srcy < 8) _
              And ((board(srcy, srcx) = "K"c And color = "w"c) _
                 Or (board(srcy, srcx) = "k"c And color = "b"c)) 
            srcSquare = Convert.ToChar(Convert.ToInt32("a"c) + srcx) + Convert.ToString(8 - srcy)
          End If
          srcx = dstx - 1
          srcy = dsty - 1
          If (srcx >= 0 And srcx < 8) And (srcy >= 0 And srcy < 8) _
              And ((board(srcy, srcx) = "K"c And color = "w"c) _
                 Or (board(srcy, srcx) = "k"c And color = "b"c)) 
            srcSquare = Convert.ToChar(Convert.ToInt32("a"c) + srcx) + Convert.ToString(8 - srcy)
          End If
        Case "Q"c
        Case "R"c
        Case "B"c
        Case "N"c
          srcx = dstx + 1
          srcy = dsty + 2
          If (srcx >= 0 And srcx < 8) And (srcy >= 0 And srcy < 8) _
              And ((board(srcy, srcx) = "N"c And color = "w"c) _
                 Or (board(srcy, srcx) = "n"c And color = "b"c)) 
            trySrc = Convert.ToChar(Convert.ToInt32("a"c) + srcx) + Convert.ToString(8 - srcy)
            If disambiguation <> Nothing And trySrc.Contains(disambiguation)
              srcSquare = trySrc
              disambiguation = Nothing
            Else
              If disambiguation = Nothing And Not srcSquare Is Nothing
                Return Nothing
              End If 
              srcSquare = trySrc
            End If
          End If
          srcx = dstx - 1
          srcy = dsty + 2
          If (srcx >= 0 And srcx < 8) And (srcy >= 0 And srcy < 8) _
              And ((board(srcy, srcx) = "N"c And color = "w"c) _
                 Or (board(srcy, srcx) = "n"c And color = "b"c)) 
            trySrc = Convert.ToChar(Convert.ToInt32("a"c) + srcx) + Convert.ToString(8 - srcy)
            If disambiguation <> Nothing And trySrc.Contains(disambiguation)
              srcSquare = trySrc
              disambiguation = Nothing
            Else
              If disambiguation = Nothing And Not srcSquare Is Nothing
                Return Nothing
              End If 
              srcSquare = trySrc
            End If
          End If
          srcx = dstx + 1
          srcy = dsty - 2
          If (srcx >= 0 And srcx < 8) And (srcy >= 0 And srcy < 8) _
              And ((board(srcy, srcx) = "N"c And color = "w"c) _
                 Or (board(srcy, srcx) = "n"c And color = "b"c)) 
            trySrc = Convert.ToChar(Convert.ToInt32("a"c) + srcx) + Convert.ToString(8 - srcy)
            If disambiguation <> Nothing And trySrc.Contains(disambiguation)
              srcSquare = trySrc
              disambiguation = Nothing
            Else
              If disambiguation = Nothing And Not srcSquare Is Nothing
                Return Nothing
              End If 
              srcSquare = trySrc
            End If
          End If
          srcx = dstx - 1
          srcy = dsty - 2
          If (srcx >= 0 And srcx < 8) And (srcy >= 0 And srcy < 8) _
              And ((board(srcy, srcx) = "N"c And color = "w"c) _
                 Or (board(srcy, srcx) = "n"c And color = "b"c))  
            trySrc = Convert.ToChar(Convert.ToInt32("a"c) + srcx) + Convert.ToString(8 - srcy)
            If disambiguation <> Nothing And trySrc.Contains(disambiguation)
              srcSquare = trySrc
              disambiguation = Nothing
            Else
              If disambiguation = Nothing And Not srcSquare Is Nothing
                Return Nothing
              End If 
              srcSquare = trySrc
            End If
          End If
          srcx = dstx + 2
          srcy = dsty + 1
          If (srcx >= 0 And srcx < 8) And (srcy >= 0 And srcy < 8)  _
              And ((board(srcy, srcx) = "N"c And color = "w"c) _
                 Or (board(srcy, srcx) = "n"c And color = "b"c))   
            trySrc = Convert.ToChar(Convert.ToInt32("a"c) + srcx) + Convert.ToString(8 - srcy)
            If disambiguation <> Nothing And trySrc.Contains(disambiguation)
              srcSquare = trySrc
              disambiguation = Nothing
            Else
              If disambiguation = Nothing And Not srcSquare Is Nothing
                Return Nothing
              End If 
              srcSquare = trySrc
            End If
          End If
          srcx = dstx - 2
          srcy = dsty + 1
          If (srcx >= 0 And srcx < 8) And (srcy >= 0 And srcy < 8) _
              And ((board(srcy, srcx) = "N"c And color = "w"c) _
                 Or (board(srcy, srcx) = "n"c And color = "b"c))   
            trySrc = Convert.ToChar(Convert.ToInt32("a"c) + srcx) + Convert.ToString(8 - srcy)
            If disambiguation <> Nothing And trySrc.Contains(disambiguation)
              srcSquare = trySrc
              disambiguation = Nothing
            Else
              If disambiguation = Nothing And Not srcSquare Is Nothing
                Return Nothing
              End If 
              srcSquare = trySrc
            End If
          End If
          srcx = dstx - 1
          srcy = dsty + 2
          If (srcx >= 0 And srcx < 8) And (srcy >= 0 And srcy < 8) _
              And ((board(srcy, srcx) = "N"c And color = "w"c) _
                 Or (board(srcy, srcx) = "n"c And color = "b"c))   
            trySrc = Convert.ToChar(Convert.ToInt32("a"c) + srcx) + Convert.ToString(8 - srcy)
            If disambiguation <> Nothing And trySrc.Contains(disambiguation)
              srcSquare = trySrc
              disambiguation = Nothing
            Else
              If disambiguation = Nothing And Not srcSquare Is Nothing
                Return Nothing
              End If 
              srcSquare = trySrc
            End If
          End If
          srcx = dstx - 1
          srcy = dsty - 2
          If (srcx >= 0 And srcx < 8) And (srcy >= 0 And srcy < 8) _
              And ((board(srcy, srcx) = "N"c And color = "w"c) _
                 Or (board(srcy, srcx) = "n"c And color = "b"c))   
            trySrc = Convert.ToChar(Convert.ToInt32("a"c) + srcx) + Convert.ToString(8 - srcy)
            If disambiguation <> Nothing And trySrc.Contains(disambiguation)
              srcSquare = trySrc
              disambiguation = Nothing
            Else
              If disambiguation = Nothing And Not srcSquare Is Nothing
                Return Nothing
              End If 
              srcSquare = trySrc
            End If
          End If
        Case "P"c
          If pieceCaptured
          Else
            srcx = dstx
            srcy = dsty + pawnSgn
            If (board(srcy, srcx) = "P"c And color = "w"c) _
                 Or (board(srcy, srcx) = "p"c And color = "b"c)
              srcSquare = Convert.ToChar(Convert.ToInt32("a"c) + srcx) + Convert.ToString(8 - srcy)
            End If
            If (dsty = 4 And color = "w"c) Or (dsty = 3 And color = "b"c)
              If board(srcy, srcx) <> " "c
                Return Nothing
              End If
              srcy += pawnSgn
              If (board(srcy, srcx) = "P"c And color = "w"c) _
                 Or (board(srcy, srcx) = "p"c And color = "b"c)
                srcSquare = Convert.ToChar(Convert.ToInt32("a"c) + srcx) + Convert.ToString(8 - srcy)
              End If
            End If
          End If
      End Select
    End If

    ' Check disambiguity. It should have been resolved
    ' and set to Nothing or remain empty
    If srcSquare Is Nothing Or disambiguation <> Nothing
      Return Nothing
    End If

    ' Do the move
    srcx = Convert.ToInt32(Convert.ToInt32(srcSquare(0)) - Convert.ToInt32("a"c))
    srcy = 8 - Val(srcSquare(1))
    board(dsty, dstx) = board(srcy, srcx)
    board(srcy, srcx) = " "c

    ' Check if the current color's king is in check (Unroll if the king is in check)

    ' Commit the move
    color = If(color = "w"c, "b"c, "w"c)
    If piece = "P"c And (srcy - dsty) * (srcy - dsty) = 4
      enPassant = Convert.ToChar(Convert.ToInt32("a"c) + dstx) + Convert.ToString(8 - dsty - pawnSgn)
    Else
      enPassant = "-"
    End If
    If pieceCaptured
      Dim reservePiece = piece
      For i = 0 To promotedSquares.Count - 1
        If promotedSquares(i) = destSquare
          promotedSquares(i) = promotedSquares(promotedSquares.Count - 1)
          promotedSquares.RemoveAt(promotedSquares.Count - 1)
          reservePiece = "P"c
          Exit For
        End If
      Next
      reserve.Add(reservePiece)
    End If
    For i = 0 To promotedSquares.Count - 1
      If promotedSquares(i) = srcSquare
        promotedSquares(i) = destSquare
        Exit For
      End If
    Next
    ' Then, check for both check and checkmate to get suffix
    Return ""
  End Function
End Class

End Namespace