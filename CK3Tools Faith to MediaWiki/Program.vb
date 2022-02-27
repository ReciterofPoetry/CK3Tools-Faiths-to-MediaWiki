Imports System.IO
Imports CK3Tools_Faiths_to_MediaWiki.BaseData
Imports Microsoft.Win32
Friend Module Props
    Property BaseDir As String = "D:\Programs\Steam\steamapps\workshop\content\1158310\2326030123"
    'Property BaseDir As String = "D:\Programs\Steam\steamapps\common\Crusader Kings III\game"
    'Property BaseDir As String = Environment.CurrentDirectory
End Module
Module Program
#Disable Warning IDE0044 ' Add readonly modifier
    Dim Groups, Categories, Doctrines As New List(Of String) 'These are the raw ids of the doctrines which will later be named. The later SortedLists will refer back to the indexes of these.
    Dim Tenets As New List(Of String) 'These will be the raw id of tenets which are just doctrines but there can be multiple of them instead of just one from each category. All tenets go into the Tenets Group.
    Dim GroupOfCategory, CategoryOfDoctrine As New Dictionary(Of Integer, Integer) 'These Dictionaries have the Category and the Doctrine as keys, with the values being their parent.
    Dim Blocks As New List(Of String) 'Store non-parsed data from files that contain religion data.
    Dim BuggedDoctrinesTenets As New List(Of String) 'This will store any doctrines or tenets that were not found in the game files allowing skipping over these in code.
    Dim GameConceptLocalisations As New SortedList(Of String, String)
#Enable Warning IDE0044 ' Add readonly modifier
    Sub Main()

        Dim FileList As List(Of String) = Directory.GetFiles(BaseDir & "\common\religion\religions", "*.txt", SearchOption.AllDirectories).ToList 'Get all doctrine code files.

        Dim Families, Religions, Faiths As New List(Of String) 'Raw ids of religion that will later be named.
        Dim ReligionDoctrines, FamilyReligions, ReligionFaiths, FaithDoctrines As New Dictionary(Of Integer, String) 'These dictionaries will contain families, religions, and faiths as keys with the values being their data.
        Dim FamilyOfReligion, ReligionOfFaith As New Dictionary(Of Integer, Integer) 'These dictionaries will have religion-family and faith-religion as the key-value pairs to find the parent family or religion quickly.

        For Each TextFile In FileList
            Dim Text As List(Of String) = File.ReadAllText(TextFile).Split(vbCrLf & "}", StringSplitOptions.RemoveEmptyEntries).ToList 'Split the file contents into the religion nested blocks.

            Text.RemoveAll(Function(x) Not x.Contains("{"c) OrElse x.StartsWith("#"c)) 'Remove any that don't seem to contain any data by searching for lack of curly brackets.

            For Each Block In Text
                Dim Religion As String = Block.Split({"="c, "{"c}, 2)(0).Trim.Split({vbCrLf, vbTab, " "c}, StringSplitOptions.None).Last 'Get the raw id of the religion.

                Religions.Add(Religion) 'Add the religion then get the index.
                Religion = Religions.Count - 1
                ReligionDoctrines.Add(Religion, "")

                Dim Family As String = Block.Split("family", 2).Last.Split("="c, 2).Last.Split(vbCrLf, 2).First.Trim 'Get the raw id of the family.

                If Not Families.Contains(Family) Then 'Add family to families list and dictionary if not added, then get the index.
                    Families.Add(Family)
                    Family = Families.Count - 1
                    FamilyReligions.Add(Family, Religion)
                Else
                    Family = Families.IndexOf(Family)
                    FamilyReligions(Family) &= " " & Religion
                End If
                FamilyOfReligion.Add(Religion, Family) 'Add the religion-family pair to this dictionary.

                Dim RawDoctrines As List(Of String) = Block.Split("faiths", 2)(0).Split(vbCrLf, StringSplitOptions.TrimEntries).ToList.FindAll(Function(x) x.StartsWith("doctrine") AndAlso x.Contains("="c) AndAlso Not x.StartsWith("doctrine_")) 'Get the doctrines of the religion but not child faiths.

                For Each Doctrine In RawDoctrines
                    Doctrine = Doctrine.Split("="c, 2)(1).Trim.Split(" "c).First
                    If Not BuggedDoctrinesTenets.Contains(Doctrine) Then
                        If Doctrines.Contains(Doctrine) Then 'Make sure doctrine exists in database. Doctrines are added on-demand. Then get index of doctrine.
                            Doctrine = Doctrines.IndexOf(Doctrine)
                        ElseIf Tenets.Contains(Doctrine) Then
                            Debug.Print("Tenet wrongly assigned at religion level: " & Doctrine)
                            Doctrine = -1 'Tenets cannot be assigned at religion level, only faith level. -1 signifies not to add it.
                        Else
                            AddDoctrine(Doctrine)
                            If Doctrines.Contains(Doctrine) Then
                                Doctrine = Doctrines.Count - 1
                            Else
                                Doctrine = -1 'If doctrine is tenet or else does not exist, then -1 signifies it should not be added.
                            End If
                        End If

                        If Not Doctrine.Split(":").Last = -1 Then
                            ReligionDoctrines(Religion) &= $" {Doctrine}" 'Add the doctrine of religion into the dictionary.
                        End If
                    End If
                Next
                ReligionDoctrines(Religion) = ReligionDoctrines(Religion).TrimStart

                'Extracting the overall block of faiths.

                Dim RawFaithsBlock As String = Block.Split("faiths", 2)(1).Split("="c, 2)(1).Split("{"c, 2)(1) 'Get all text after 'faiths = {'
                Do 'Determine the subsidiary {} curly bracket blocks and designate them as such by replacing them with angle brackets.
                    RawFaithsBlock = String.Join(">", String.Join("<", RawFaithsBlock.Split("{"c, 2)).Split("}"c, 2))
                Loop While RawFaithsBlock.Split("}", 2)(0).Contains("{"c) 'Loop until there is no } closing bracket that has a { starting bracket before it remaining. The next } closing bracket remaining will be for the faiths block.
                RawFaithsBlock = RawFaithsBlock.Split("}"c)(0).Replace("<", "{").Replace(">", "}") 'Split off any code after that last } closing bracket and then replace the angle brackets back with curly brackets.

                Dim RawFaiths As New SortedList(Of String, String) 'The individual faith blocks will be parsed into this SortedList.
                Do
                    Dim RawFaith As String = RawFaithsBlock.Split("{", 2)(0) 'Get the faith id.
                    Dim RawFaithBlock As String = Block.Split(RawFaith, 2)(1).Split("{"c, 2)(1) 'Get the rest of the block after the faith id.
                    RawFaith = RawFaith.Split("="c, 2)(0).Trim.Split().Last 'Remove unnecessary code in faith id.
                    Do 'Same old method to designate subsidiary curly brackets as such with angle brackets.
                        RawFaithBlock = String.Join(">", String.Join("<", RawFaithBlock.Split("{"c, 2)).Split("}"c, 2))
                    Loop While RawFaithBlock.Split("}", 2)(0).Contains("{"c) 'Loop until no more subsidiary code.
                    RawFaithBlock = RawFaithBlock.Split("}"c)(0).Replace("<", "{").Replace(">", "}") 'Get the data of this faith by splitting off whatever remains after its { closing bracket.
                    RawFaiths.Add(RawFaith, RawFaithBlock) 'Add to SortedList
                    RawFaithsBlock = RawFaithsBlock.Split(RawFaithBlock, 2)(1).Split("}", 2)(1) 'Remove already parsed data from the rest of the unparsed data.
                Loop While RawFaithsBlock.Split("}", 2)(0).Contains("{"c) 'Continue to parse the data until no more faiths can be found by looking for a { starting bracket.

                For Each Faith In RawFaiths 'Now parse the collected faith data.
                    Faiths.Add(Faith.Key) 'Add the faith id to list.
                    Dim FaithIndex As Integer = Faiths.Count - 1 'Collect the index of the faith.
                    FaithDoctrines.Add(FaithIndex, "")

                    If Not ReligionFaiths.ContainsKey(Religion) Then 'Add to Dictionary of religion-'subsidiary faiths' key-value pairs, referring to the indexes of each from the original lists.
                        ReligionFaiths.Add(Religion, FaithIndex)
                    Else
                        ReligionFaiths(Religion) &= $" {FaithIndex}"
                    End If

                    ReligionOfFaith.Add(FaithIndex, Religion) 'Add to the Dictionary of faith-'parent religion' key-value pairs, referring to indexes.

                    RawDoctrines = Faith.Value.Split(vbCrLf, StringSplitOptions.TrimEntries).ToList.FindAll(Function(x) x.StartsWith("doctrine") AndAlso x.Contains("="c) AndAlso Not x.StartsWith("doctrine_")) 'Get the doctrines of the faith.

                    For Each Doctrine In RawDoctrines
                        Doctrine = Doctrine.Split("="c, 2)(1).Trim.Split(" "c).First
                        If Not BuggedDoctrinesTenets.Contains(Doctrine) Then
                            If Doctrines.Contains(Doctrine) Then 'Make sure doctrine exists in database. Doctrines are added on-demand. Then get index of doctrine. If tenets, do the same, then get index of tenet while signifying it is a tenet with 't:' prefixed.
                                Doctrine = Doctrines.IndexOf(Doctrine)
                            ElseIf Tenets.Contains(Doctrine) Then
                                Doctrine = "t:" & Tenets.IndexOf(Doctrine)
                            Else
                                AddDoctrine(Doctrine)
                                If Doctrines.Contains(Doctrine) Then
                                    Doctrine = Doctrines.Count - 1
                                ElseIf Tenets.Contains(Doctrine) Then
                                    Doctrine = "t:" & Tenets.Count - 1
                                Else
                                    Doctrine = -1 '-1 signifies that the doctrine does not exist in game code.
                                End If
                            End If

                            If Not Doctrine.Split(":").Last = -1 Then
                                FaithDoctrines(FaithIndex) &= $" {Doctrine}" 'Add doctrine of faith into the dictionary.
                            End If
                        End If
                    Next
                    FaithDoctrines(FaithIndex) = FaithDoctrines(FaithIndex).TrimStart
                Next
            Next
        Next

        Dim ReligionDescs As List(Of String) = Religions.ToList 'Clone religions into ReligionDescs to enable searching for descriptions by appending '_desc' to them.
        Dim FaithDescs As List(Of String) = Faiths.ToList 'Clone faiths into FaithDescs to enable searching for descriptions by appending '_desc' to them.

        Dim LocalisationFiles As List(Of String) = Directory.GetFiles(BaseDir & "\localization\english", "*.yml", SearchOption.AllDirectories).Concat(Directory.GetFiles(BaseDir & "\localization\replace\english", "*.yml", SearchOption.AllDirectories)).ToList

        Dim RawGameConceptLocalisations As New List(Of String)
        For Each TextFile In LocalisationFiles
            Using SR As New StreamReader(TextFile)
                Dim LineData As String
                While Not SR.EndOfStream
                    LineData = SR.ReadLine
                    If LineData Like "*game_concept*" AndAlso Not LineData Like "*$game_concept*" Then
                        RawGameConceptLocalisations = RawGameConceptLocalisations.Concat(File.ReadAllLines(TextFile)).ToList
                        Exit While
                    End If
                End While
            End Using
        Next

        For Each Item In RawGameConceptLocalisations
            If Item Like "*game_concept*" Then
                Dim GameConcept As String = Item.Split(":")(0).Split("game_concept_")(1)
                If Not GameConceptLocalisations.ContainsKey(GameConcept) Then
                    GameConceptLocalisations.Add(Item.Split(":")(0).Split("game_concept_")(1), Item.Split(Chr(34))(1))
                Else
                    GameConceptLocalisations(GameConcept) = Item.Split(Chr(34))(1)
                End If
            End If
        Next

        Dim RawLocalisation As List(Of String) = VanillaLoc()

        GetLocalisation(Categories, RawLocalisation, LocalisationFiles, "_name")
        GetLocalisation(Doctrines, RawLocalisation, LocalisationFiles, "_name")
        GetLocalisation(Tenets, RawLocalisation, LocalisationFiles, "_name")
        GetLocalisation(Families, RawLocalisation, LocalisationFiles)
        GetLocalisation(Religions, RawLocalisation, LocalisationFiles)
        GetLocalisation(ReligionDescs, RawLocalisation, LocalisationFiles, "_desc")
        GetLocalisation(Faiths, RawLocalisation, LocalisationFiles)
        GetLocalisation(FaithDescs, RawLocalisation, LocalisationFiles, "_desc")

        Dim OutputFile As String = File.ReadAllLines(BaseDir & "/descriptor.mod").ToList.Find(Function(x) x.StartsWith("name=")).Split(Chr(34), 3)(1)
        OutputFile = $"{Environment.GetFolderPath(Environment.SpecialFolder.Desktop)}\{String.Concat(OutputFile.Split(Path.GetInvalidFileNameChars))} Faiths.txt"
        Using SW As New StreamWriter(OutputFile)
            For Count = 0 To Families.Count - 1
                SW.WriteLine($"== List of {Families(Count)} faiths ==")
                Dim ChildReligions As List(Of String) = FamilyReligions(Count).Split.ToList
                For Each Religion In ChildReligions
                    SW.WriteLine($"=== {Religions(Religion)} ===")
                    SW.WriteLine($"{vbCrLf}{ReligionDescs(Religion).Replace("/n", vbCrLf).Replace("\n", vbCrLf).Replace("/", "").Replace("\", "")}{vbCrLf}")
                    SW.WriteLine("<tabber>")
                    SW.WriteLine(" Game information=")
                    SW.WriteLine("{| class=""wikitable sortable""")
                    SW.WriteLine($"! Faith !! Tenets !! {String.Join(" !! ", Groups.FindAll(Function(x) Not x.Equals("not_creatable")))}")
                    Dim ChildFaiths As List(Of String) = ReligionFaiths(Religion).Split.ToList
                    Dim ChildReligionDoctrines As List(Of String) = ReligionDoctrines(Religion).Split(" "c, StringSplitOptions.RemoveEmptyEntries).ToList
                    For Each Faith In ChildFaiths
                        SW.WriteLine("|-")
                        SW.WriteLine($"| style=""text-align: center;"" | {Faiths(Faith)}")
                    SW.WriteLine("| ")
                        Dim ChildTenets As List(Of String) = FaithDoctrines(Faith).Split.ToList.FindAll(Function(x) x.StartsWith("t:"))
                        For Each Tenet In ChildTenets
                            SW.WriteLine($"* {Tenets(Tenet.Split(":").Last)}")
                        Next
                        Dim ChildFaithDoctrines As List(Of String) = FaithDoctrines(Faith).Split(" "c, StringSplitOptions.RemoveEmptyEntries).ToList.FindAll(Function(x) Not x.StartsWith("t:"))
                        Dim ChildFaithCategories As New List(Of String)
                        For Each Doctrine In ChildFaithDoctrines
                            ChildFaithCategories.Add(CategoryOfDoctrine(Doctrine))
                        Next
                        For Each Doctrine In ChildReligionDoctrines
                            If Not ChildFaithCategories.Contains(CategoryOfDoctrine(Doctrine)) Then
                                ChildFaithDoctrines.Add(Doctrine)
                            End If
                        Next

                        Dim ChildGroups As New SortedList(Of Integer, String)
                        For GCount = 0 To Groups.Count - 1
                            If Not Groups(GCount).Contains("not_creatable") Then
                                ChildGroups.Add(GCount, "")
                            End If
                        Next
                        For Each Doctrine In ChildFaithDoctrines
                            Dim Group As Integer = GroupOfCategory(CategoryOfDoctrine(Doctrine))
                            If ChildGroups.ContainsKey(Group) Then
                                ChildGroups(Group) &= $" {Doctrine}"
                            End If
                        Next
                        For Each Group In ChildGroups
                            Dim ChildDoctrines As List(Of String) = Group.Value.TrimStart.Split(" "c, StringSplitOptions.RemoveEmptyEntries).ToList
                            SW.WriteLine($"| ")
                            For Each Doctrine In ChildDoctrines
                                If Not Groups(Group.Key) = "special" Then
                                    SW.WriteLine($"* {Categories(CategoryOfDoctrine(Doctrine))}: {Doctrines(Doctrine)}")
                                Else
                                    SW.WriteLine($"* {Doctrines(Doctrine)}")
                                End If
                            Next
                        Next
                    Next
                    SW.WriteLine("|}")
                    SW.WriteLine("|-|")
                    SW.WriteLine(" Lore=")
                    SW.WriteLine("{| class=""wikitable sortable"" style=""width=100%;""")
                    SW.WriteLine("! Faith !! Description")
                    For Each Faith In ChildFaiths
                        SW.WriteLine("|-")
                        SW.WriteLine($"| style=""text-align: center;"" | {Faiths(Faith)}")
                        SW.WriteLine($"| {FaithDescs(Faith).Replace("/n", vbCrLf).Replace("\n", vbCrLf).Replace("/", "").Replace("\", "")}")
                    Next
                    SW.WriteLine("|}")
                    SW.WriteLine("</tabber>")
                Next
            Next
            'If CultureDescriptions.Count = 0 Then
            '    SW.WriteLine("! Heritage !! Culture !! Ethos !! Traditions !! Language !! Martial Custom")
            'Else
            '    SW.WriteLine("! Heritage !! Culture !! Ethos !! Traditions !! Language !! Martial Custom !! Description")
            'End If
            'For Each Heritage In HeritageCultures.Keys
            '    Dim Value As String
            '    Dim CultureList As List(Of String) = HeritageCultures(Heritage).Split().ToList
            '    SW.WriteLine($"|-{vbCrLf}| rowspan=""{CultureList.Count}"" | '''{Heritages(Heritage)}'''")
            '    For Count = 0 To CultureList.Count - 1
            '        Dim Culture As Integer = CultureList(Count)
            '        If CultureDescriptions.Count = 0 Then
            '            CultureList(Count) = $"!style=""background:rgb({String.Join(", ", Colours(Culture).Split)})""| {Cultures(Culture)}{vbCrLf}| {Ethoses(Culture)}{vbCrLf}| {vbCrLf}{Traditions(Culture)}{vbCrLf}| {Languages(Culture)}{vbCrLf}| {MartialCustoms(Culture)}"
            '        Else
            '            CultureList(Count) = $"!style=""background:rgb({String.Join(", ", Colours(Culture).Split)})""| {Cultures(Culture)}{vbCrLf}| {Ethoses(Culture)}{vbCrLf}| {vbCrLf}{Traditions(Culture)}{vbCrLf}| {Languages(Culture)}{vbCrLf}| {MartialCustoms(Culture)}{vbCrLf}| {String.Join(vbCrLf, CultureDescriptions(Culture).Replace("/n", vbCrLf).Replace("\n", vbCrLf).Replace("\", "").Split(vbCrLf, StringSplitOptions.TrimEntries))}"
            '        End If
            '    Next
            '    SW.WriteLine(String.Join($"{vbCrLf}|-{vbCrLf}", CultureList))
            'Next
        End Using

        Console.WriteLine("Successfully deposited output to desktop. Press any key to close.")
        Console.ReadKey()
    End Sub
    Private Sub AddDoctrine(Doctrine As String)
        Dim FileList As List(Of String) = Directory.GetFiles(BaseDir & "\common\religion\doctrines", "*.txt", SearchOption.AllDirectories).ToList 'Get all doctrine code files.

        Dim TextBlock As String = "" 'The string to which the block containing relevant data will be assigned to.

        If Blocks.Exists(Function(x) x.Split(vbCrLf, StringSplitOptions.TrimEntries).ToList.Exists(Function(y) y.StartsWith(Doctrine))) Then 'Search for the data in already accessed game data first.
            TextBlock = Blocks.Find(Function(x) x.Split(vbCrLf, StringSplitOptions.TrimEntries).ToList.Exists(Function(y) y.StartsWith(Doctrine & " ") OrElse y.StartsWith(Doctrine & "=") OrElse y.StartsWith(Doctrine & "{")))
        Else 'Search for the data in game files not yet accessed.
            For Each TextFile In FileList
                Dim Text As List(Of String) = File.ReadAllText(TextFile).Split(vbCrLf & "}", StringSplitOptions.RemoveEmptyEntries).ToList 'Split the file contents into the doctrine category nested blocks.
                Text.RemoveAll(Function(x) Not x.Contains("{"c)) 'Remove any that don't seem to contain any data by searching for lack of curly brackets.

                If Text.Exists(Function(x) x.Split(vbCrLf, StringSplitOptions.TrimEntries).ToList.Exists(Function(y) y.StartsWith(Doctrine))) Then
                    For Each Block In Text
                        If Block.Split(vbCrLf, StringSplitOptions.TrimEntries).ToList.Exists(Function(x) x.StartsWith(Doctrine)) Then
                            TextBlock = Block
                            Blocks.AddRange(Text)
                            Exit For 'Terminate iterations when appropriate data is found. Hold file data in memory.
                        End If
                    Next
                    Exit For
                End If
            Next
        End If

        If Not TextBlock = "" Then
            Dim Test As String
            If TextBlock.Contains("number_of_picks") Then
                Test = TextBlock.Split("number_of_picks", 2)(1).Split("="c)(1).Split(" "c, StringSplitOptions.RemoveEmptyEntries).First
            End If
            If TextBlock.Contains("number_of_picks") AndAlso TextBlock.Split("number_of_picks", 2)(1).Split("="c)(1).Split({" "c, vbTab, vbCrLf}, StringSplitOptions.RemoveEmptyEntries).First > 1 Then 'If there are multiple picks then these are assumed to be tenets.
                Tenets.Add(Doctrine)
            Else
                Dim Category As String = TextBlock.Split({"="c, "{"c}, 2)(0).Trim.Split({vbCrLf, vbTab, " "c}, StringSplitOptions.None).Last 'Get the raw id of the category.

                Dim Group As String = TextBlock.Split("group", 2)(1).Split(Chr(34), 3)(1) 'Get the raw id of the group.

                If Not Groups.Contains(Group) Then 'Make sure the raw group id is added, then make the string the index.
                    Groups.Add(Group)
                    Group = Groups.Count - 1
                Else
                    Group = Groups.IndexOf(Group)
                End If

                If Not Categories.Contains(Category) Then 'Make sure the raw category id is added, then make the string the index.
                    Categories.Add(Category)
                    Category = Categories.Count - 1
                    GroupOfCategory.Add(Category, Group)
                Else
                    Category = Categories.IndexOf(Category)
                End If

                Doctrines.Add(Doctrine) 'Add doctrine, and its paired category finally.
                CategoryOfDoctrine.Add(Doctrines.Count - 1, Category)

            End If
        Else 'If mod files don't contain data, then look for data in pre-parsed base game files.
            If BaseTenets.Contains(Doctrine) Then 'Base game tenets
                Tenets.Add(Doctrine)
            ElseIf BaseDoctrines.Contains(Doctrine) Then 'Base game doctrines.
                Doctrines.Add(Doctrine)
                Dim BaseDoctrine As Integer = BaseDoctrines.IndexOf(Doctrine)
                Doctrine = Doctrines.Count - 1
                Dim Category As String = BaseCategoryOfDoctrine(BaseDoctrine) 'Get the index of category in base data.
                Dim Group As String = BaseGroupOfCategory(Category) 'Get the index of group in base data.
                Category = BaseCategories(Category) 'Get the code id of category from base data.
                Group = BaseGroups(Group) 'Get the code if of group from base data.
                If Categories.Contains(Category) Then 'If the category already exists, then add it as parent/value to doctrine in dictionary.
                    CategoryOfDoctrine.Add(Doctrine, Categories.IndexOf(Category))
                Else 'Add new category and sort it accordingly.
                    Categories.Add(Category)
                    Category = Categories.Count - 1
                    CategoryOfDoctrine.Add(Doctrine, Category)
                    If Groups.Contains(Group) Then
                        GroupOfCategory.Add(Category, Groups.IndexOf(Group))
                    Else
                        Groups.Add(Group)
                        GroupOfCategory.Add(Category, Groups.Count - 1)
                    End If
                End If
            Else
                Debug.Print("Doctrine not found: " & Doctrine) 'Either there is a bug in this code or a bug in the mod.
                BuggedDoctrinesTenets.Add(Doctrine)
            End If
        End If
    End Sub
    Private Sub GetLocalisation(ByRef Code As List(Of String), ByRef RawLocalisation As List(Of String), ByRef LocalisationFiles As List(Of String), Optional Suffix As String = "")
        For Count = 0 To Code.Count - 1
            If Not Code(Count) = "" Then
                Dim RawCode As String = Code(Count) & Suffix
                If Not RawLocalisation.Exists(Function(x) x.TrimStart.StartsWith($"{RawCode}:")) Then
                    For Each TextFile In LocalisationFiles
                        Using SR As New StreamReader(TextFile)
                            While Not SR.EndOfStream
                                Dim LineData As String = SR.ReadLine
                                If LineData.TrimStart.StartsWith($"{RawCode}:") Then
                                    RawLocalisation = RawLocalisation.Concat(File.ReadAllLines(TextFile)).ToList
                                    Code(Count) = LineData.Split(Chr(34), 3)(1)
                                    If Code(Count).Contains("#"c) Then
                                        Code(Count) = DeFormat(Code(Count))
                                    End If
                                    If Code(Count).Contains("|E]") Then
                                        Code(Count) = DeConcept(Code(Count))
                                    End If
                                    Exit For
                                End If
                            End While
                        End Using
                    Next
                Else
                    Code(Count) = RawLocalisation.FindLast(Function(x) x.TrimStart.StartsWith($"{RawCode}:")).Split(Chr(34), 3)(1)
                    If Code(Count).Contains("#"c) Then
                        Code(Count) = DeFormat(Code(Count))
                    End If
                    If Code(Count).Contains("|E]") Then
                        Code(Count) = DeConcept(Code(Count))
                    End If
                    If Code(Count).Contains("$") Then
                        Dim RawLoc As List(Of String) = {Code(Count)}.ToList
                        Do
                            RawLoc(0) = RawLoc(0).Split("$"c, 2).Last
                            Code(Count) = Code(Count).Replace($"${Code(Count).Split("$"c, 3)(1)}$", RawLoc(0))
                            GetLocalisation(RawLoc, RawLocalisation, LocalisationFiles)
                        Loop While RawLoc(0).Contains("$")
                    End If
                End If
            End If
        Next
    End Sub
    Function DeConcept(Input As String) As String
        Dim Output As String = Input
        Do
            Dim GameConcept As String = Split(Split(Output, "[", 2)(1), "|", 2)(0)
            Dim Finder As String
            If GameConceptLocalisations.ContainsKey(GameConcept.ToLower) Then
                Finder = GameConceptLocalisations(GameConcept.ToLower)
            Else
                Finder = GameConcept
            End If
            Output = Output.Replace($"[{GameConcept}|E]", Finder)
        Loop While Output.Contains("|E]")
        Return Output
    End Function
    Function DeFormat(Input As String) As String
        Dim Output As List(Of String) = Input.Split("#"c, StringSplitOptions.RemoveEmptyEntries).ToList
        For Count = 0 To Output.Count - 1
            Dim Formatted() As String = Output(Count).Split({" "c, vbTab}, 2, StringSplitOptions.None)
            Output(Count) = Output(Count).Split({" "c, vbTab}, 2, StringSplitOptions.None)(1)
        Next
        Return String.Concat(Output).Trim
    End Function
End Module
Friend Module BaseData
    Property BaseGroups As New List(Of String) From {"core_tenets", "marriage", "crimes", "main_group", "clergy", "not_creatable", "special"}
    Property BaseCategories As New List(Of String) From {"doctrine_core_tenets", "doctrine_marriage_type", "doctrine_divorce", "doctrine_bastardry", "doctrine_homosexuality", "doctrine_deviancy", "doctrine_adultery_men", "doctrine_adultery_women", "doctrine_kinslaying", "doctrine_witchcraft", "doctrine_gender", "doctrine_consanguinity", "doctrine_pluralism", "doctrine_theocracy", "doctrine_head_of_faith", "doctrine_clerical_function", "doctrine_clerical_gender", "doctrine_clerical_marriage", "doctrine_clerical_succession", "doctrine_muhammad_succession", "hostility_group", "is_christian_faith", "is_islamic_faith", "is_jewish_faith", "is_eastern_faith", "is_gnostic_faith", "special_tolerance", "heresy_hostility", "nudity_doctrine", "unreformed_faith", "divine_destiny", "full_tolerance"}
    Property BaseDoctrines As New List(Of String) From {"doctrine_monogamy", "doctrine_polygamy", "doctrine_concubines", "doctrine_divorce_disallowed", "doctrine_divorce_approval", "doctrine_divorce_allowed", "doctrine_bastardry_none", "doctrine_bastardry_legitimization", "doctrine_bastardry_all", "doctrine_homosexuality_crime", "doctrine_homosexuality_shunned", "doctrine_homosexuality_accepted", "doctrine_deviancy_crime", "doctrine_deviancy_shunned", "doctrine_deviancy_accepted", "doctrine_adultery_men_crime", "doctrine_adultery_men_shunned", "doctrine_adultery_men_accepted", "doctrine_adultery_women_crime", "doctrine_adultery_women_shunned", "doctrine_adultery_women_accepted", "doctrine_kinslaying_any_dynasty_member_crime", "doctrine_kinslaying_extended_family_crime", "doctrine_kinslaying_close_kin_crime", "doctrine_kinslaying_shunned", "doctrine_kinslaying_accepted", "doctrine_witchcraft_crime", "doctrine_witchcraft_shunned", "doctrine_witchcraft_accepted", "doctrine_gender_male_dominated", "doctrine_gender_equal", "doctrine_gender_female_dominated", "doctrine_consanguinity_restricted", "doctrine_consanguinity_cousins", "doctrine_consanguinity_aunt_nephew_and_uncle_niece", "doctrine_consanguinity_unrestricted", "doctrine_pluralism_fundamentalist", "doctrine_pluralism_righteous", "doctrine_pluralism_pluralistic", "doctrine_theocracy_temporal", "doctrine_theocracy_lay_clergy", "doctrine_no_head", "doctrine_spiritual_head", "doctrine_temporal_head", "doctrine_clerical_function_taxation", "doctrine_clerical_function_alms_and_pacification", "doctrine_clerical_function_recruitment", "doctrine_clerical_gender_male_only", "doctrine_clerical_gender_female_only", "doctrine_clerical_gender_either", "doctrine_clerical_marriage_allowed", "doctrine_clerical_marriage_disallowed", "doctrine_clerical_succession_temporal_appointment", "doctrine_clerical_succession_spiritual_appointment", "doctrine_clerical_succession_temporal_fixed_appointment", "doctrine_clerical_succession_spiritual_fixed_appointment", "muhammad_succession_sunni_doctrine", "muhammad_succession_shia_doctrine", "muhammad_succession_muhakkima_doctrine", "muhammad_succession_zandaqa_doctrine", "abrahamic_hostility_doctrine", "pagan_hostility_doctrine", "eastern_hostility_doctrine", "special_doctrine_is_christian_faith", "special_doctrine_is_islamic_faith", "special_doctrine_is_jewish_faith", "special_doctrine_is_eastern_faith", "special_doctrine_is_gnostic_faith", "special_doctrine_ecumenical_christian", "heresy_hostility_doctrine", "special_doctrine_naked_priests", "unreformed_faith_doctrine", "divine_destiny_doctrine", "special_doctrine_full_tolerance"}
    'Property BaseCategoriesGrouped As New Dictionary(Of Integer, String) From {{0, "0"}, {1, "1 2 3 11"}, {2, "4 5 6 7 8 9"}, {3, "10 12 13 14 19 26"}, {4, "15 16 17 18"}, {5, "20 27 29"}, {6, "21 22 23 24 25 28 30 31"}}
    'Property BaseDoctrinesCategorised As New Dictionary(Of Integer, String) From {{1, "0 1 2"}, {2, "3 4 5"}, {3, "6 7 8"}, {4, "9 10 11"}, {5, "12 13 14"}, {6, "15 16 17"}, {7, "18 19 20"}, {8, "21 22 23 24 25"}, {9, "26 27 28"}, {10, "29 30 31"}, {11, "32 33 34 35"}, {12, "36 37 38"}, {13, "39 40"}, {14, "41 42 43"}, {15, "44 45 46"}, {16, "47 48 49"}, {17, "50 51"}, {18, "52 53 54 55"}}
    Property BaseGroupOfCategory As New Dictionary(Of Integer, Integer) From {{0, 0}, {1, 1}, {2, 1}, {3, 1}, {4, 2}, {5, 2}, {6, 2}, {7, 2}, {8, 2}, {9, 2}, {10, 3}, {11, 1}, {12, 3}, {13, 3}, {14, 3}, {15, 4}, {16, 4}, {17, 4}, {18, 4}, {19, 3}, {20, 5}, {21, 6}, {22, 6}, {23, 6}, {24, 6}, {25, 6}, {26, 3}, {27, 5}, {28, 6}, {29, 5}, {30, 6}, {31, 6}}
    Property BaseCategoryOfDoctrine As New Dictionary(Of Integer, Integer) From {{0, 1}, {1, 1}, {2, 1}, {3, 2}, {4, 2}, {5, 2}, {6, 3}, {7, 3}, {8, 3}, {9, 4}, {10, 4}, {11, 4}, {12, 5}, {13, 5}, {14, 5}, {15, 6}, {16, 6}, {17, 6}, {18, 7}, {19, 7}, {20, 7}, {21, 8}, {22, 8}, {23, 8}, {24, 8}, {25, 8}, {26, 9}, {27, 9}, {28, 9}, {29, 10}, {30, 10}, {31, 10}, {32, 11}, {33, 11}, {34, 11}, {35, 11}, {36, 12}, {37, 12}, {38, 12}, {39, 13}, {40, 13}, {41, 14}, {42, 14}, {43, 14}, {44, 15}, {45, 15}, {46, 15}, {47, 16}, {48, 16}, {49, 16}, {50, 17}, {51, 17}, {52, 18}, {53, 18}, {54, 18}, {55, 18}, {56, 19}, {57, 19}, {58, 19}, {59, 19}, {60, 20}, {61, 20}, {62, 20}, {63, 21}, {64, 22}, {65, 23}, {66, 24}, {67, 25}, {68, 26}, {69, 27}, {70, 28}, {71, 29}, {72, 30}, {73, 31}}
    Property BaseTenets As New List(Of String) From {"tenet_aniconism", "tenet_alexandrian_catechism", "tenet_armed_pilgrimages", "tenet_carnal_exaltation", "tenet_communal_identity", "tenet_communion", "tenet_consolamentum", "tenet_divine_marriage", "tenet_gnosticism", "tenet_mendicant_preachers", "tenet_monasticism", "tenet_pacifism", "tenet_pentarchy", "tenet_unrelenting_faith", "tenet_vows_of_poverty", "tenet_pastoral_isolation", "tenet_adaptive", "tenet_esotericism", "tenet_legalism", "tenet_literalism", "tenet_reincarnation", "tenet_religious_legal_pronouncements", "tenet_struggle_submission", "tenet_false_conversion_sanction", "tenet_tax_nonbelievers", "tenet_asceticism", "tenet_bhakti", "tenet_dharmic_pacifism", "tenet_inner_journey", "tenet_ritual_hospitality", "tenet_adorcism", "tenet_ancestor_worship", "tenet_astrology", "tenet_hedonistic", "tenet_human_sacrifice", "tenet_mystical_birthright", "tenet_ritual_celebrations", "tenet_sacred_childbirth", "tenet_sanctity_of_nature", "tenet_sky_burials", "tenet_sun_worship", "tenet_warmonger", "tenet_gruesome_festivals", "tenet_eastern_syncretism", "tenet_unreformed_syncretism", "tenet_christian_syncretism", "tenet_islamic_syncretism", "tenet_jewish_syncretism", "tenet_exaltation_of_pain", "tenet_natural_primitivism", "tenet_pursuit_of_power", "tenet_ritual_cannibalism", "tenet_sacred_shadows", "tenet_polyamory"}
    Function VanillaLoc() As List(Of String)
        Return New List(Of String)({
" doctrine_core_tenets_name:1 ""Tenets""",
" doctrine_marriage_type_name:0 ""Marriage Type""",
" doctrine_divorce_name:0 ""Divorce""",
" doctrine_bastardry_name:0 ""Bastardry""",
" doctrine_homosexuality_name:1 ""Same-Sex Relations""",
" doctrine_deviancy_name:0 ""Deviancy""",
" doctrine_adultery_men_name:0 ""Male Adultery""",
" doctrine_adultery_women_name:0 ""Female Adultery""",
" doctrine_kinslaying_name:0 ""Kinslaying""",
" doctrine_witchcraft_name:0 ""Witchcraft""",
" doctrine_gender_name:0 ""View on Gender""",
" doctrine_consanguinity_name:0 ""Consanguinity""",
" doctrine_pluralism_name:0 ""Religious Attitude""",
" doctrine_theocracy_name:0 ""Clerical Tradition""",
" doctrine_head_of_faith_name:0 ""Head of Faith""",
" doctrine_clerical_function_name:0 ""Clerical Function""",
" doctrine_clerical_gender_name:0 ""Clerical Gender""",
" doctrine_clerical_marriage_name:0 ""Clerical Marriage""",
" doctrine_clerical_succession_name:0 ""Clerical Appointment""",
" doctrine_muhammad_succession_name:0 ""Muhammad's Succession""",
" hostility_group_name:0 ""Hostility Group""",
" is_christian_faith_name:0 ""Teachings of Jesus""",
" is_islamic_faith_name:0 ""Teachings of the Prophet""",
" is_jewish_faith_name:0 ""Teachings of Moses""",
" is_eastern_faith_name:0 ""Multireligious Interweaving""",
" is_gnostic_faith_name:0 ""$tenet_gnosticism_name$""",
" special_tolerance_name:0 ""Special Tolerance""",
" heresy_hostility_name:0 ""View on Heresy""",
" nudity_doctrine_name:0 ""View on Nudity""",
" unreformed_faith_name:0 ""Unreformed""",
" divine_destiny_name:0 ""Divine Destiny""",
" full_tolerance_name:0 ""Full Tolerance""",
" doctrine_monogamy_name:0 ""Monogamous""",
" doctrine_polygamy_name:0 ""Polygamous""",
" doctrine_concubines_name:1 ""Concubines""",
" doctrine_divorce_disallowed_name:0 ""Disallowed""",
" doctrine_divorce_approval_name:0 ""Must be Approved""",
" doctrine_divorce_allowed_name:0 ""Always Allowed""",
" doctrine_bastardry_none_name:0 ""No Bastards""",
" doctrine_bastardry_legitimization_name:0 ""Legitimization""",
" doctrine_bastardry_all_name:0 ""No Legitimization""",
" doctrine_homosexuality_crime_name:1 ""Criminal""",
" doctrine_homosexuality_shunned_name:1 ""Shunned""",
" doctrine_homosexuality_accepted_name:1 ""Accepted""",
" doctrine_deviancy_crime_name:0 ""Criminal""",
" doctrine_deviancy_shunned_name:0 ""Shunned""",
" doctrine_deviancy_accepted_name:0 ""Accepted""",
" doctrine_adultery_men_crime_name:0 ""Criminal""",
" doctrine_adultery_men_shunned_name:0 ""Shunned""",
" doctrine_adultery_men_accepted_name:0 ""Accepted""",
" doctrine_adultery_women_crime_name:0 ""Criminal""",
" doctrine_adultery_women_shunned_name:0 ""Shunned""",
" doctrine_adultery_women_accepted_name:0 ""Accepted""",
" doctrine_kinslaying_any_dynasty_member_crime_name:0 ""Dynastic is Criminal""",
" doctrine_kinslaying_extended_family_crime_name:0 ""Familial is Criminal""",
" doctrine_kinslaying_close_kin_crime_name:0 ""Close Kin is Criminal""",
" doctrine_kinslaying_shunned_name:0 ""Shunned""",
" doctrine_kinslaying_accepted_name:0 ""Accepted""",
" doctrine_witchcraft_crime_name:0 ""Criminal""",
" doctrine_witchcraft_shunned_name:0 ""Shunned""",
" doctrine_witchcraft_accepted_name:0 ""Accepted""",
" doctrine_gender_male_dominated_name:0 ""Male Dominated""",
" doctrine_gender_equal_name:0 ""Equal""",
" doctrine_gender_female_dominated_name:0 ""Female Dominated""",
" doctrine_consanguinity_restricted_name:0 ""Close-kin Taboo""",
" doctrine_consanguinity_cousins_name:0 ""Cousin Marriage""",
" doctrine_consanguinity_aunt_nephew_and_uncle_niece_name:0 ""Avunculate Marriage""",
" doctrine_consanguinity_unrestricted_name:0 ""Unrestricted Marriage""",
" doctrine_pluralism_fundamentalist_name:0 ""Fundamentalist""",
" doctrine_pluralism_righteous_name:0 ""Righteous""",
" doctrine_pluralism_pluralistic_name:0 ""Pluralist""",
" doctrine_theocracy_temporal_name:0 ""Theocratic""",
" doctrine_theocracy_lay_clergy_name:0 ""Lay Clergy""",
" doctrine_no_head_name:0 ""None""",
" doctrine_spiritual_head_name:0 ""Spiritual""",
" doctrine_temporal_head_name:0 ""Temporal""",
" doctrine_clerical_function_taxation_name:1 ""Control""",
" doctrine_clerical_function_alms_and_pacification_name:0 ""Alms and Pacification""",
" doctrine_clerical_function_recruitment_name:0 ""Recruitment""",
" doctrine_clerical_gender_male_only_name:0 ""Only Men""",
" doctrine_clerical_gender_female_only_name:0 ""Only Women""",
" doctrine_clerical_gender_either_name:0 ""Either""",
" doctrine_clerical_marriage_allowed_name:0 ""Allowed""",
" doctrine_clerical_marriage_disallowed_name:0 ""Disallowed""",
" doctrine_clerical_succession_temporal_appointment_name:0 ""Temporal, Revocable""",
" doctrine_clerical_succession_spiritual_appointment_name:0 ""Spiritual, Revocable""",
" doctrine_clerical_succession_temporal_fixed_appointment_name:0 ""Temporal, for Life""",
" doctrine_clerical_succession_spiritual_fixed_appointment_name:0 ""Spiritual, for Life""",
" muhammad_succession_sunni_doctrine_name:0 ""Sunni""",
" muhammad_succession_shia_doctrine_name:0 ""Shia""",
" muhammad_succession_muhakkima_doctrine_name:0 ""Muhakkima""",
" muhammad_succession_zandaqa_doctrine_name:0 ""Zandaqa""",
" abrahamic_hostility_doctrine_name:0 ""Abrahamic""",
" pagan_hostility_doctrine_name:0 ""Pagan""",
" eastern_hostility_doctrine_name:0 ""Eastern""",
" special_doctrine_is_christian_faith_name:0 ""Teachings of Jesus""",
" special_doctrine_is_islamic_faith_name:0 ""Teachings of the Prophet""",
" special_doctrine_is_jewish_faith_name:0 ""Teachings of Moses""",
" special_doctrine_is_eastern_faith_name:0 ""Multireligious Interweaving""",
" special_doctrine_is_gnostic_faith_name:0 ""Gnosticism""",
" special_doctrine_ecumenical_christian_name:0 ""Ecumenism""",
" heresy_hostility_doctrine_name:0 ""Heresy""",
" special_doctrine_naked_priests_name:0 ""Naked Priests""",
" unreformed_faith_doctrine_name:0 ""Unreformed""",
" divine_destiny_doctrine_name:0 ""Rightful Rulers of the World""",
" special_doctrine_full_tolerance_name:0 ""Full Tolerance""",
" tenet_aniconism_name:0 ""Aniconism""",
" tenet_alexandrian_catechism_name:0 ""Alexandrian Catechism""",
" tenet_armed_pilgrimages_name:0 ""Armed Pilgrimages""",
" tenet_carnal_exaltation_name:0 ""Carnal Exaltation""",
" tenet_communal_identity_name:0 ""Communal Identity""",
" tenet_communion_name:0 ""Communion""",
" tenet_consolamentum_name:0 ""Consolamentum""",
" tenet_divine_marriage_name:0 ""Divine Marriage""",
" tenet_gnosticism_name:0 ""Gnosticism""",
" tenet_mendicant_preachers_name:0 ""Mendicant Preachers""",
" tenet_monasticism_name:0 ""Monasticism""",
" tenet_pacifism_name:0 ""Pacifism""",
" tenet_pentarchy_name:0 ""Ecclesiarchy""",
" tenet_unrelenting_faith_name:0 ""Unrelenting Faith""",
" tenet_vows_of_poverty_name:0 ""Vows of Poverty""",
" tenet_pastoral_isolation_name:0 ""Pastoral Isolation""",
" tenet_adaptive_name:0 ""Adaptive""",
" tenet_esotericism_name:0 ""Esotericism""",
" tenet_legalism_name:0 ""Legalism""",
" tenet_literalism_name:0 ""Literalism""",
" tenet_reincarnation_name:0 ""Reincarnation""",
" tenet_religious_legal_pronouncements_name:0 ""Religious Law""",
" tenet_struggle_submission_name:0 ""Struggle and Submission""",
" tenet_false_conversion_sanction_name:0 ""Sanctioned False Conversions""",
" tenet_tax_nonbelievers_name:0 ""Tax Nonbelievers""",
" tenet_asceticism_name:0 ""Asceticism""",
" tenet_bhakti_name:0 ""Bhakti""",
" tenet_dharmic_pacifism_name:0 ""Dharmic Pacifism""",
" tenet_inner_journey_name:0 ""Inner Journey""",
" tenet_ritual_hospitality_name:0 ""Ritual Hospitality""",
" tenet_adorcism_name:0 ""Adorcism""",
" tenet_ancestor_worship_name:0 ""Ancestor Worship""",
" tenet_astrology_name:0 ""Astrology""",
" tenet_hedonistic_name:0 ""Hedonistic""",
" tenet_human_sacrifice_name:0 ""Human Sacrifice""",
" tenet_mystical_birthright_name:0 ""Auspicious Birthright""",
" tenet_ritual_celebrations_name:0 ""Ritual Celebrations""",
" tenet_sacred_childbirth_name:0 ""Sacred Childbirth""",
" tenet_sanctity_of_nature_name:0 ""Sanctity of Nature""",
" tenet_sky_burials_name:0 ""Sky Burials""",
" tenet_sun_worship_name:0 ""Sun Worship""",
" tenet_warmonger_name:0 ""Warmonger""",
" tenet_gruesome_festivals_name:0 ""Gruesome Festivals""",
" tenet_eastern_syncretism_name:0 ""Eastern Syncretism""",
" tenet_unreformed_syncretism_name:0 ""Syncretic Folk Traditions""",
" tenet_christian_syncretism_name:0 ""Christian Syncretism""",
" tenet_islamic_syncretism_name:0 ""Islamic Syncretism""",
" tenet_jewish_syncretism_name:0 ""Jewish Syncretism""",
" tenet_exaltation_of_pain_name:0 ""Exaltation of Pain""",
" tenet_natural_primitivism_name:0 ""Natural Primitivism""",
" tenet_pursuit_of_power_name:0 ""Pursuit of Power""",
" tenet_ritual_cannibalism_name:0 ""Ritual Cannibalism""",
" tenet_sacred_shadows_name:0 ""Sacred Lies""",
" tenet_polyamory_name:0 ""Polyamory"""})
    End Function
End Module