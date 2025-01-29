Imports System.ComponentModel
Imports System.Data
Imports System.Runtime.CompilerServices
Imports ConsoleApp1.Test
Imports DeveloperCore.NotifyChanged

Public Module Program
    Public Sub Main(args As String())
    End Sub
End Module

Namespace Other.Nice
    Public Class Customer
        Inherits BindableObject

        Private _customer As BackingCustomer

        <NotifyChanged>
        <EmitBind(NameOf(_customer))>
        <EmitCall(NameOf(Test1))>
        <EmitCall(NameOf(Test2))> Private _name As String

        <NotifyChanged>
        <EmitBind(NameOf(_customer))>
        <EmitCondition(NameOf(Cond1))>
        <EmitCondition(NameOf(Cond2))> Private _age As Integer

        <NotifyChanged> Private m_strAddress As String

        <NotifyChanged> Private _items As DataTable

        <NotifyChanged> Private _children As List(Of List(Of BackingCustomer))

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
End Namespace

Namespace Test
    Public Class BackingCustomer
        Public Property Name As String
        Public Property Age As Integer
    End Class
End Namespace

Public MustInherit Class BindableObject
    Implements INotifyPropertyChanged, INotifyPropertyChanging

    Public Sub RaiseAndSetIfChanged(Of T)(ByRef field As T, value As T, <CallerMemberName> Optional propertyName As String = "")
        If Not EqualityComparer(Of T).Default.Equals(field, value) Then
            RaiseNotifyPropertyChanging(propertyName)
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