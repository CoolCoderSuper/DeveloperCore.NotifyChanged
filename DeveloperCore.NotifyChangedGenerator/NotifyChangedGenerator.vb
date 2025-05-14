Imports System.Threading
Imports Microsoft.CodeAnalysis
Imports Microsoft.CodeAnalysis.VisualBasic
Imports S = Microsoft.CodeAnalysis.VisualBasic.SyntaxFactory
Imports Microsoft.CodeAnalysis.VisualBasic.Syntax

<Generator(LanguageNames.VisualBasic)>
Public Class NotifyChangedGenerator
    Implements IIncrementalGenerator

    Public Sub Initialize(initContext As IncrementalGeneratorInitializationContext) Implements IIncrementalGenerator.Initialize
        Dim values = initContext.SyntaxProvider.ForAttributeWithMetadataName("DeveloperCore.NotifyChanged.NotifyChangedAttribute", Function(node, token) True, AddressOf ProcessNotifyChangedAttribute)
        initContext.RegisterSourceOutput(values, AddressOf Generate)
    End Sub

    Private Sub Generate(context As SourceProductionContext, info As PropertyGenInfo)
        context.AddSource(info.Name & ".g.vb", info.Syntax.ToFullString())
    End Sub

    Private Function ProcessNotifyChangedAttribute(context As GeneratorAttributeSyntaxContext, token As CancellationToken) As PropertyGenInfo
        Dim fieldSymbol As IFieldSymbol = context.TargetSymbol
        Dim attr = context.Attributes.First(Function(x) x.AttributeClass.Name = "NotifyChangedAttribute")
        Dim providedName As String = attr.NamedArguments.FirstOrDefault(Function(x) x.Key = "Name").Value.Value
        Dim actualName = If(providedName, GetPropertyName(context.TargetSymbol.Name))
        Dim hasSetterMeth = context.TargetSymbol.ContainingType.GetMembers().Any(Function(x) x.Name = $"Set{actualName}")
        Dim hasSetterPreCondMeth = context.TargetSymbol.ContainingType.GetMembers().Any(Function(x) x.Name = $"BeforeSet{actualName}")
        Dim methodsToCall = context.TargetSymbol.GetAttributes().Where(Function(x) x.AttributeClass.Name = "EmitCallAttribute").Select(Function(x) CStr(x.ConstructorArguments.First().Value)).ToArray()
        methodsToCall = methodsToCall.Concat(GetGlobalCalls(context.TargetSymbol.ContainingType)).ToArray()
        Dim conditionalMethods = context.TargetSymbol.GetAttributes().Where(Function(x) x.AttributeClass.Name = "EmitConditionAttribute").Select(Function(x) CStr(x.ConstructorArguments.First().Value)).ToArray()
        Dim hasAnyPreCond = conditionalMethods.Any() OrElse hasSetterPreCondMeth
        Dim bindingAttr = context.TargetSymbol.GetAttributes().FirstOrDefault(Function(x) x.AttributeClass.Name = "EmitBindAttribute")
        Dim hasBinding = bindingAttr IsNot Nothing
        Dim propertyGetter =
                S.GetAccessorBlock(S.GetAccessorStatement()).
                    AddStatements(S.ReturnStatement(S.ParseName(context.TargetSymbol.Name)))
        Dim valueSetStatement As StatementSyntax = S.ExpressionStatement(
            S.InvocationExpression(S.ParseName("RaiseAndSetIfChanged")).
                AddArgumentListArguments({
                    S.SimpleArgument(S.IdentifierName(context.TargetSymbol.Name)),
                    S.SimpleArgument(S.IdentifierName("Value"))
                })
        )
        Dim methodsToCallInvocations As New List(Of StatementSyntax)
        If hasBinding Then
            Dim bindingSource As String = bindingAttr.ConstructorArguments(0).Value
            Dim bindingProperty As String = If(bindingAttr.ConstructorArguments.Length > 1, If(bindingAttr.ConstructorArguments(1).Value, actualName), actualName)
            methodsToCallInvocations.Add(
                    S.SimpleAssignmentStatement(
                        S.SimpleMemberAccessExpression(S.IdentifierName(bindingSource), S.IdentifierName(bindingProperty)),
                        S.IdentifierName("Value")
                    )
            )
        End If
        If hasSetterMeth Then
            methodsToCallInvocations.Add(
                    S.ExpressionStatement(
                        S.InvocationExpression(S.ParseName($"Set{actualName}")).
                            AddArgumentListArguments({S.SimpleArgument(S.IdentifierName("Value"))})
                    )
            )
        End If
        For Each method In methodsToCall
            methodsToCallInvocations.Add(
                    S.ExpressionStatement(
                        S.InvocationExpression(S.ParseName(method))
                    )
            )
        Next
        If hasAnyPreCond Then
            Dim conditionalExpr As ExpressionSyntax
            If conditionalMethods.Length = 1 Then
                conditionalExpr = S.InvocationExpression(S.ParseName(conditionalMethods(0)))
            ElseIf conditionalMethods.Length > 1 Then
                conditionalExpr = S.AndAlsoExpression(S.InvocationExpression(S.ParseName(conditionalMethods(0))), S.InvocationExpression(S.ParseName(conditionalMethods(1))))
                For i As Integer = 2 To conditionalMethods.Length - 1
                    conditionalExpr = S.AndAlsoExpression(conditionalExpr, S.InvocationExpression(S.ParseName(conditionalMethods(i))))
                Next
            End If
            If hasSetterPreCondMeth Then
                Dim preCondMeth =
                        S.InvocationExpression(S.ParseName($"BeforeSet{actualName}")).
                            AddArgumentListArguments({
                                S.SimpleArgument(S.IdentifierName("Value")),
                                S.SimpleArgument(S.IdentifierName(context.TargetSymbol.Name))
                            })
                If conditionalExpr Is Nothing Then
                    conditionalExpr = preCondMeth
                Else
                    conditionalExpr = S.AndAlsoExpression(preCondMeth, conditionalExpr)
                End If
            End If
            valueSetStatement = 
                S.MultiLineIfBlock(
                    S.IfStatement(
                        conditionalExpr
                    )
                ).AddStatements(valueSetStatement).AddStatements(methodsToCallInvocations.ToArray())
        End If
        Dim propertySetter = 
                S.SetAccessorBlock(S.SetAccessorStatement()).
                    AddStatements(valueSetStatement)
        If Not hasAnyPreCond Then
            propertySetter = propertySetter.AddStatements(methodsToCallInvocations.ToArray())
        End If
        Dim propertyBlock =
                S.PropertyBlock(
                    S.PropertyStatement(actualName).
                        WithModifiers(S.TokenList(S.Token(SyntaxKind.PublicKeyword))).
                        WithAsClause(S.SimpleAsClause(S.ParseTypeName(fieldSymbol.Type.ToString()))),
                    S.List(Of AccessorBlockSyntax)({propertyGetter, propertySetter})
                )
        Dim typeName = context.TargetSymbol.ContainingType.Name
        Dim classStatement = S.ClassStatement(typeName).
                AddModifiers(S.Token(SyntaxKind.PublicKeyword), S.Token(SyntaxKind.PartialKeyword))
        Dim classBlock = S.ClassBlock(classStatement).AddMembers(propertyBlock)
        Dim result As SyntaxNode = classBlock
        Dim root As INamespaceSymbol = context.SemanticModel.Compilation.RootNamespace()
        If root.Name <> context.TargetSymbol.ContainingNamespace.Name Then
            result = S.NamespaceBlock(S.NamespaceStatement(S.ParseName(GetFullNamespace(context.TargetSymbol.ContainingNamespace, root)))).AddMembers(classBlock)
        End If
        Dim unit = S.CompilationUnit().
                AddImports(context.TargetNode.SyntaxTree.GetRoot().DescendantNodes().OfType(Of ImportsStatementSyntax).ToArray()).
                AddMembers(result)
        Return New PropertyGenInfo(unit.NormalizeWhitespace(), $"{fieldSymbol.ContainingType.Name}_{actualName}")
    End Function
    
    Private Shared Function GetGlobalCalls(symbol As INamedTypeSymbol) As String()
        Dim result As New List(Of String)
        While symbol IsNot Nothing
            result.AddRange(symbol.GetMembers().Where(Function(x) x.GetAttributes().Any(Function(y) y.AttributeClass.Name = "GlobalCallAttribute")).Select(Function(x) x.Name))
            symbol = symbol.BaseType
        End While
        Return result.ToArray()
    End Function

    Private Shared Function GetFullNamespace(symbol As INamespaceSymbol, root As INamespaceSymbol) As String
        If symbol Is Nothing OrElse symbol.Name = root.Name Then Return ""
        Dim nextName As String = GetFullNamespace(symbol.ContainingNamespace, root)
        If String.IsNullOrEmpty(nextName) Then
            Return symbol.Name
        End If
        Return $"{nextName}.{symbol.Name}"
    End Function
    
    Private Shared Function GetPropertyName(name As String) As String
        If name.StartsWith("_") Then
            name = name.Substring(1)
        ElseIf name.StartsWith("m_") Then
            name = name.Substring(2)
            name = name.Remove(0, name.IndexOf(name.First(Function(x) Char.IsUpper(x))))
        End If
        name = $"{Char.ToUpper(name(0))}{name.Remove(0, 1)}"
        If S.ParseToken(name).IsReservedKeyword() Then
            name = $"[{name}]"
        End If
        Return name
    End Function
End Class

Public Class PropertyGenInfo
    Sub New(syntax As SyntaxNode, name As String)
        Me.Syntax = syntax
        Me.Name = name
    End Sub

    Public ReadOnly Property Name As String
    Public ReadOnly Property Syntax As SyntaxNode
End Class