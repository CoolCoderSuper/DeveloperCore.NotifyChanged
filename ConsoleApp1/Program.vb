Imports System.ComponentModel
Imports System.Runtime.CompilerServices
Imports DeveloperCore.NotifyChanged

Public Module Program
    Public Sub Main(args As String())
    End Sub
End Module

Public Class Customer
    Inherits BindableObject

    <NotifyChanged>
    <EmitCall(NameOf(Test1))>
    <EmitCall(NameOf(Test2))>
    Private _name As String
    <NotifyChanged>
    <EmitCondition(NameOf(Cond1))>
    <EmitCondition(NameOf(Cond2))>
    Private _age As Integer
    
    <NotifyChanged>
    Private m_strAddress As String
    
    Private Function BeforeSetName(value As String, oldValue As String) As Boolean
        Return _name.Length > 0
    End Function

    Private Function BeforeSetAge(value As Integer, oldValue As Integer) As Boolean
        Return _name.Length > 0
    End Function
    
    Private Sub SetAge(value As Integer)
        Console.WriteLine(value)
    End Sub
    
    Private Sub Test1()
        
    End Sub
    
    Private Sub Test2()
        
    End Sub
    
    Private Function Cond1() As Boolean
        Return True
    End Function
    
    Private Function Cond2() As Boolean
        Return True
    End Function 
End Class

Public MustInherit Class BindableObject
    Implements INotifyPropertyChanged, INotifyPropertyChanging

    Public Sub RaiseAndSetIfChanged(Of T)(ByRef field As T, value As T, <CallerMemberName> Optional propertyName As String = "")
        If Not EqualityComparer(Of T).Default.Equals(field, value) Then
            RaiseNotifyPropertyChanging("")
            field = value
            RaiseNotifyPropertyChanged(propertyName)
        End If
    End Sub

    Public Sub RaiseNotifyPropertyChanging(propertyName As String)
        RaiseEvent PropertyChanging(Me, New PropertyChangingEventArgs(propertyName))
    End Sub

    Public Sub RaiseNotifyPropertyChanged(propertyName As String)
        RaiseEvent PropertyChanged(Me, New PropertyChangedEventArgs(propertyName))
    End Sub

    Public Event PropertyChanged As PropertyChangedEventHandler Implements INotifyPropertyChanged.PropertyChanged
    Public Event PropertyChanging As PropertyChangingEventHandler Implements INotifyPropertyChanging.PropertyChanging
End Class