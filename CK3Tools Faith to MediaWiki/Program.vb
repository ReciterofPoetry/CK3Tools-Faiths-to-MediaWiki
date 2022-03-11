Imports System.IO
Friend Module Props
    'Property BaseDir As String = "D:\Programs\Steam\steamapps\workshop\content\1158310\2216659254"
    'Property BaseDir As String = "D:\Programs\Steam\steamapps\common\Crusader Kings III\game"
    Property BaseDir As String = Environment.CurrentDirectory
    Property GameDir As String
End Module
Module Program
#Disable Warning IDE0044 ' Add readonly modifier
    Dim Groups, Categories, Doctrines As New List(Of String) 'These are the raw ids of the doctrines which will later be named. The later SortedLists will refer back to the indexes of these.
    Dim Tenets As New List(Of String) 'These will be the raw id of tenets which are just doctrines but there can be multiple of them instead of just one from each category. All tenets go into the Tenets Group.
    Dim GroupOfCategory, CategoryOfDoctrine As New Dictionary(Of Integer, Integer) 'These Dictionaries have the Category and the Doctrine as keys, with the values being their parent.
    Dim Blocks As New List(Of String) 'Store non-parsed data from files that contain religion data.
    Dim BuggedDoctrinesTenets As New List(Of String) 'This will store any doctrines or tenets that were not found in the game files allowing skipping over these in code.
    Dim GameConceptLocalisations As New Hashtable()
    Dim SavedLocalisation As New Hashtable()
    Dim LocalisationFiles As List(Of String)
#Enable Warning IDE0044 ' Add readonly modifier
    Sub Main()

        SetGameDir()

        Dim FileList As List(Of String) = Directory.GetFiles(BaseDir & "\common\religion\religions", "*.txt", SearchOption.AllDirectories).ToList 'Get all doctrine code files.

        Dim Families, Religions, Faiths, FaithIcons, FaithsHolySites As New List(Of String) 'Raw ids of religion that will later be named.
        Dim ReligionDoctrines, FamilyReligions, ReligionFaiths, FaithDoctrines As New Dictionary(Of Integer, List(Of String)) 'These dictionaries will contain families, religions, and faiths as keys with the values being their data.
        Dim FamilyOfReligion, ReligionOfFaith As New Dictionary(Of Integer, Integer) 'These dictionaries will have religion-family and faith-religion as the key-value pairs to find the parent family or religion quickly.

        For Each TextFile In FileList
            Dim Text As List(Of String) = DeNest(File.ReadAllText(TextFile)) 'Split the file contents into the religion nested blocks.

            Text.RemoveAll(Function(x) Not x.Contains("{"c) OrElse x.StartsWith("#"c)) 'Remove any that don't seem to contain any data by searching for lack of curly brackets.

            For Each Block In Text
                If Block.Contains("#"c) Then
                    Block = String.Join(vbCrLf, Block.Split({vbCrLf, vbCr, vbLf}, StringSplitOptions.None).ToList.FindAll(Function(x) Not x.TrimStart.StartsWith("#"c)))
                End If

                Dim Religion As String = Block.Split({"="c, "{"c}, 2)(0).Trim.Split({vbCrLf, vbTab, " "c}, StringSplitOptions.None).Last 'Get the raw id of the religion.

                Religions.Add(Religion) 'Add the religion then get the index.
                Religion = Religions.Count - 1
                ReligionDoctrines.Add(Religion, New List(Of String))
                ReligionFaiths.Add(Religion, New List(Of String))

                Dim Family As String = Block.Split("family", 2).Last.Split("="c, 2).Last.Split({vbCrLf, vbCr, vbLf}, 2, StringSplitOptions.None).First.Trim 'Get the raw id of the family.

                If Not Families.Contains(Family) Then 'Add family to families list and dictionary if not added, then get the index.
                    Families.Add(Family)
                    Family = Families.Count - 1
                    FamilyReligions.Add(Family, New List(Of String))
                Else
                    Family = Families.IndexOf(Family)
                End If
                FamilyReligions(Family).Add(Religion)
                FamilyOfReligion.Add(Religion, Family) 'Add the religion-family pair to this dictionary.

                Dim RawDoctrines As List(Of String) = Block.Split("faiths", 2)(0).Split({vbCrLf, vbCr, vbLf}, StringSplitOptions.TrimEntries).ToList.FindAll(Function(x) x.StartsWith("doctrine") AndAlso x.Contains("="c) AndAlso Not x.StartsWith("doctrine_")) 'Get the doctrines of the religion but not child faiths.

                For Each Doctrine In RawDoctrines
                    Doctrine = Doctrine.Split("="c, 2)(1).Trim.Split({" "c, vbTab, vbCrLf, vbCr, vbLf}, StringSplitOptions.None).First
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
                            ReligionDoctrines(Religion).Add(Doctrine) 'Add the doctrine of religion into the dictionary.
                        End If
                    End If
                Next

                'Extracting the overall block of faiths.
                If Block.Contains("faiths") Then
                    Dim RawFaithsBlock As String = DeNest("faiths" & Block.Split("faiths", 2).Last).First 'Get faiths object block.
                    Dim StartIndex As Integer = RawFaithsBlock.IndexOf("{"c) + 1
                    RawFaithsBlock = RawFaithsBlock.Substring(StartIndex, RawFaithsBlock.LastIndexOf("}"c) - StartIndex)
                    If RawFaithsBlock.Contains("{"c) Then 'Make sure this religion actually has faiths in its faiths block.
                        Dim RawFaiths As List(Of String) = DeNest(RawFaithsBlock).FindAll(Function(x) x.Contains("="c)) 'The individual faith blocks will be parsed into this SortedList.

                        For Each Faith In RawFaiths 'Now parse the collected faith data.
                            Dim FaithID As String = Faith.Split("{"c, 2).First.Split("="c, 2).First.TrimEnd.Split({" "c, vbTab, vbCrLf, vbCr, vbLf}, StringSplitOptions.None).Last
                            Faiths.Add(FaithID) 'Add the faith id to list.
                            Dim FaithIndex As Integer = Faiths.Count - 1 'Collect the index of the faith.
                            FaithDoctrines.Add(FaithIndex, New List(Of String))
                            FaithsHolySites.Add("")

                            'Add to Dictionary of religion-'subsidiary faiths' key-value pairs, referring to the indexes of each from the original lists.
                            ReligionFaiths(Religion).Add(FaithIndex)

                            ReligionOfFaith.Add(FaithIndex, Religion) 'Add to the Dictionary of faith-'parent religion' key-value pairs, referring to indexes.

                            RawDoctrines = Faith.Split({vbCrLf, vbCr, vbLf}, StringSplitOptions.TrimEntries).ToList.FindAll(Function(x) x.StartsWith("doctrine") AndAlso x.Contains("="c) AndAlso Not x.StartsWith("doctrine_")) 'Get the doctrines of the faith.

                            For Each Doctrine In RawDoctrines
                                Doctrine = Doctrine.Split("="c, 2)(1).Trim.Split({" "c, vbTab, vbCrLf, vbCr, vbLf}, StringSplitOptions.None).First
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
                                        FaithDoctrines(FaithIndex).Add(Doctrine) 'Add doctrine of faith into the dictionary.
                                    End If
                                End If
                            Next

                            Dim Icon As String = ""
                            If Faith.Contains("icon") Then
                                Icon = Faith.Split({vbCrLf, vbCr, vbLf}, StringSplitOptions.None).ToList.Find(Function(x) x.Contains("icon") AndAlso x.Contains("="c)).Split("icon", 2).Last.Split("="c, 2).Last.Trim.Split({" "c, vbTab, vbCrLf, vbCr, vbLf}, StringSplitOptions.None).First.Replace(".dds", "")
                            Else
                                Icon = FaithID
                            End If
                            FaithIcons.Add(Icon)

                            Dim RawHolySites As List(Of String) = Faith.Split({vbCrLf, vbCr, vbLf}, StringSplitOptions.TrimEntries).ToList.FindAll(Function(x) x.StartsWith("holy_site") AndAlso x.Contains("="c)) 'Get the holy sites of the faith.
                            For Count = 0 To RawHolySites.Count - 1
                                RawHolySites(Count) = RawHolySites(Count).Split("="c, 2).Last.Trim.Split(" "c, 2).First
                            Next
                            FaithsHolySites(FaithIndex) = String.Join(" "c, RawHolySites)
                        Next
                    End If
                End If
            Next
        Next

        FileList = Directory.GetFiles(GameDir & "\common\religion\holy_sites", "*.txt", SearchOption.AllDirectories).Concat(Directory.GetFiles(BaseDir & "\common\religion\holy_sites", "*.txt", SearchOption.AllDirectories)).ToList 'Get holy site code.
        Dim HolySitesBlocks As New List(Of String)
        Dim HolySites As New Dictionary(Of String, String)
        Dim HolySiteCounties As New List(Of String)

        For Each TextFile In FileList
            HolySitesBlocks = HolySitesBlocks.Concat(DeNest(File.ReadAllText(TextFile))).ToList 'Parse the raw code into the list through DeNest function.
        Next

        For Count = 0 To HolySitesBlocks.Count - 1 'Parse each block for id and county. Discard the block string and replace with the parsed information. Collect counties into a separate list for localisation later.
            Dim HolySiteID As String = HolySitesBlocks(Count).Split("{", 2).First.Split({"="c, " "c, vbCrLf, vbTab}, StringSplitOptions.RemoveEmptyEntries).Last.Trim
            Dim County As String = HolySitesBlocks(Count).Split(HolySiteID, 2).Last.Split("{", 2).Last.Split("county", 2).Last.Split("="c, 2).Last.TrimStart.Split({" "c, vbTab, vbCrLf}, StringSplitOptions.None).First
            If Not HolySiteCounties.Contains(County) Then
                HolySiteCounties.Add(County)
                County = HolySiteCounties.Count - 1
            Else
                County = HolySiteCounties.IndexOf(County)
            End If
            If Not HolySites.ContainsKey(HolySiteID) Then
                HolySites.Add(HolySiteID, County)
            Else
                HolySites(HolySiteID) = County
            End If
        Next

        For Count = 0 To FaithsHolySites.Count - 1 'Parse the holy sites listed for each faith for its associated county, then record the county's index on the HolySiteCounties list which will later be parsed with the localisation function and give us the county names.
            If Not FaithsHolySites(Count).Length = 0 Then
                Dim FaithHolySites As List(Of String) = FaithsHolySites(Count).Split.ToList
                For HSCount = 0 To FaithHolySites.Count - 1
                    If HolySites.ContainsKey(FaithHolySites(HSCount)) Then
                        FaithHolySites(HSCount) = HolySites(FaithHolySites(HSCount))
                    Else
                        Debug.Print($"Holy site not found: {FaithHolySites(HSCount)}")
                        FaithHolySites(HSCount) = -1
                    End If
                Next
                FaithHolySites.RemoveAll(Function(x) x = -1)
                FaithsHolySites(Count) = String.Join(" "c, FaithHolySites)
            End If
        Next

        Dim ReligionDescs As List(Of String) = Religions.ToList 'Clone religions into ReligionDescs to enable searching for descriptions by appending '_desc' to them.
        Dim FaithDescs As List(Of String) = Faiths.ToList 'Clone faiths into FaithDescs to enable searching for descriptions by appending '_desc' to them.

        'Localise all the data collected so far.

        CollectLocalisations()

        GetLocalisation(Categories, "_name")
        GetLocalisation(Doctrines, "_name")
        GetLocalisation(Tenets, "_name")
        GetLocalisation(Families)
        GetLocalisation(Religions)
        GetLocalisation(ReligionDescs, "_desc")
        GetLocalisation(Faiths)
        GetLocalisation(FaithDescs, "_desc")
        GetLocalisation(HolySiteCounties)

        For Count = 0 To Groups.Count - 1
            Select Case Groups(Count)
                Case "main_group"
                    Groups(Count) = "Main Doctrines"
                Case "special"
                    Groups(Count) = "Special Doctrines"
                Case "crimes"
                    Groups(Count) = "Crime Doctrines"
                Case "marriage"
                    Groups(Count) = "Marital Doctrines"
                Case "clergy"
                    Groups(Count) = "Clerical Doctrines"
            End Select
        Next

        'Write the data into a textfile according to MediaWiki markdown + the Tabber/TabberNeue extension code.

        Dim OutputFile As String
        If File.Exists(BaseDir & "/descriptor.mod") Then
            OutputFile = File.ReadAllLines(BaseDir & "/descriptor.mod").ToList.Find(Function(x) x.StartsWith("name=")).Split(Chr(34), 3)(1) 'Get the mod name from the descriptor.mod file if it exists and then name the output textfile with that.
            OutputFile = $"{Environment.GetFolderPath(Environment.SpecialFolder.Desktop)}\{String.Concat(OutputFile.Split(Path.GetInvalidFileNameChars))} Faiths.txt"
        Else 'If no descriptor.mod exists, then just name it 'Faiths to MediaWiki.txt'.
            OutputFile = $"{Environment.GetFolderPath(Environment.SpecialFolder.Desktop)}\Faiths to MediaWiki.txt"
        End If

        Using SW As New StreamWriter(OutputFile) 'The code has been written to ensure that all religions are sorted into a family and all faiths are sorted into a religion. The output will present them like that.
            For Count = 0 To Families.Count - 1 'Write religion family name as large header.
                SW.WriteLine($"== List of {Families(Count)} faiths ==")
                Dim ChildReligions As List(Of String) = FamilyReligions(Count) 'Get its religions.
                For Each Religion In ChildReligions
                    SW.WriteLine($"=== {Religions(Religion)} ===") 'Religion name, smaller header.
                    SW.WriteLine($"{vbCrLf}{ReligionDescs(Religion).Replace("/n", vbCrLf).Replace("\n", vbCrLf).Replace("/", "").Replace("\", "")}{vbCrLf}") 'Religion description. Last minute localisation: add in the new lines as Carriage Return Line Feeds. This will be transferred to the GetLocalisation function at some point.
                    SW.WriteLine("<tabber>") 'Tabber/TabberNeue code.
                    SW.WriteLine(" Game information=") 'First tab: Game Information.
                    SW.WriteLine("{| class=""wikitable sortable""") 'Table markdown, start.
                    SW.WriteLine($"! Faith !! Tenets !! {String.Join(" !! ", Groups.FindAll(Function(x) Not x.Equals("not_creatable")))} !! Holy Sites") 'Header cells. Each group except `not_creatable` added as a column.
                    Dim ChildFaiths As List(Of String) = ReligionFaiths(Religion) 'Get the religion's faiths.
                    Dim ChildReligionDoctrines As List(Of String) = ReligionDoctrines(Religion) 'Get the religion's doctrines.
                    For Each Faith In ChildFaiths 'Start writing each faith.
                        SW.WriteLine("|-") 'New row.
                        SW.WriteLine($"| style=""text-align: center;"" | {Faiths(Faith)}<br>[[File:{FaithIcons(Faith)}.png|100px]]") 'Faith name then link to its icon.
                        SW.WriteLine("| ") 'New cell.
                        Dim ChildTenets As List(Of String) = FaithDoctrines(Faith).FindAll(Function(x) x.StartsWith("t:"))
                        For Each Tenet In ChildTenets 'Tenets in bullet point form.
                            SW.WriteLine($"* {Tenets(Tenet.Split(":").Last)}")
                        Next
                        Dim ChildFaithDoctrines As List(Of String) = FaithDoctrines(Faith).FindAll(Function(x) Not x.StartsWith("t:")) 'Get doctrines.
                        Dim ChildFaithCategories As New List(Of String)
                        For Each Doctrine In ChildFaithDoctrines 'Figure out which doctrine categories are accounted for to ensure no repeats when adding the parent religion doctrines.
                            ChildFaithCategories.Add(CategoryOfDoctrine(Doctrine))
                        Next
                        For Each Doctrine In ChildReligionDoctrines 'Add in religion doctrines if possible.
                            If Not ChildFaithCategories.Contains(CategoryOfDoctrine(Doctrine)) Then
                                ChildFaithDoctrines.Add(Doctrine)
                            End If
                        Next

                        'Sort the doctrines of this faith into each group except the `not_creatable` one with a SortedList with group as the key and the doctrines as the value.

                        Dim ChildGroups As New SortedList(Of Integer, String)
                        For GCount = 0 To Groups.Count - 1 'First get the groups as the key.
                            If Not Groups(GCount).Contains("not_creatable") Then
                                ChildGroups.Add(GCount, "")
                            End If
                        Next
                        For Each Doctrine In ChildFaithDoctrines 'Then add the doctrines in as the value for each key.
                            Dim Group As Integer = GroupOfCategory(CategoryOfDoctrine(Doctrine))
                            If ChildGroups.ContainsKey(Group) Then
                                ChildGroups(Group) &= $" {Doctrine}"
                            End If
                        Next
                        For Each Group In ChildGroups 'Then write down each groups doctrines into the output in its own cell.
                            Dim ChildDoctrines As List(Of String) = Group.Value.TrimStart.Split(" "c, StringSplitOptions.RemoveEmptyEntries).ToList 'Get the doctrines into a list.
                            SW.WriteLine("| ") 'New cell.
                            For Each Doctrine In ChildDoctrines 'Write each doctrine.
                                If Not Groups(Group.Key) = "Special Doctrines" Then 'If the group is not 'special' group then category name of doctrine, then doctrine name.
                                    SW.WriteLine($"* {Categories(CategoryOfDoctrine(Doctrine))}: {Doctrines(Doctrine)}")
                                Else 'If the group is 'special' group then doctrine name. Category is not shown in-game and has no loc.
                                    SW.WriteLine($"* {Doctrines(Doctrine)}")
                                End If
                            Next
                        Next

                        SW.WriteLine("| ") 'New cell.
                        Dim FaithHolySites As List(Of String) = FaithsHolySites(Faith).Split.ToList 'Get this faith's holy sites.
                        For Each HolySite In FaithHolySites
                            If Not HolySite.Length = 0 Then 'Holy sites in bullet form.
                                SW.WriteLine($"* {HolySiteCounties(HolySite)}")
                            End If
                        Next
                    Next
                    SW.WriteLine("|}") 'Table markdown, end.
                    SW.WriteLine("|-|") 'New tab in Tabber/TabberNeue.
                    SW.WriteLine(" Lore=") 'Lore tab.
                    SW.WriteLine("{| class=""wikitable sortable""") 'Table markdown, start.
                    SW.WriteLine("! Faith !! Description") 'Faith and Description columns.
                    For Each Faith In ChildFaiths
                        SW.WriteLine("|-") 'New row.
                        SW.WriteLine($"| style=""text-align: center;"" | {Faiths(Faith)}<br>[[File:{FaithIcons(Faith)}.png|100px]]") 'Faith name as well as a link to its icon.
                        SW.WriteLine($"| {FaithDescs(Faith).Replace("/n", vbCrLf).Replace("\n", vbCrLf).Replace("/", "").Replace("\", "")}") 'Faith description.
                    Next
                    SW.WriteLine("|}") 'Table markdown, end.
                    SW.WriteLine("</tabber>") 'Tabber/TabberNeue closing code.
                Next
            Next
        End Using

        Console.WriteLine("Successfully deposited output to desktop. Press any key to close.") 'A notification before exiting.
        Console.ReadKey()
    End Sub
    Private Sub AddDoctrine(Doctrine As String)
        Dim FileList As New List(Of String)
        If Directory.Exists(BaseDir & "\common\religion\doctrines") Then
            FileList = Directory.GetFiles(BaseDir & "\common\religion\doctrines", "*.txt", SearchOption.AllDirectories).ToList 'Get all doctrine code files.
            FileList.Reverse()
        End If
        Dim BaseFiles As List(Of String) = Directory.GetFiles(GameDir & "\common\religion\doctrines", "*.txt", SearchOption.AllDirectories).ToList
        BaseFiles.Reverse()
        FileList = FileList.Concat(BaseFiles).ToList

        Dim TextBlock As String = "" 'The string to which the block containing relevant data will be assigned to.

        If Blocks.Exists(Function(x) x.Split({vbCrLf, vbCr, vbLf}, StringSplitOptions.TrimEntries).ToList.Exists(Function(y) y.StartsWith(Doctrine))) Then 'Search for the data in already accessed game data first.
            TextBlock = Blocks.Find(Function(x) x.Split({vbCrLf, vbCr, vbLf}, StringSplitOptions.TrimEntries).ToList.Exists(Function(y) y.StartsWith(Doctrine & " ") OrElse y.StartsWith(Doctrine & "=") OrElse y.StartsWith(Doctrine & "{")))
        Else 'Search for the data in game files not yet accessed.
            For Each TextFile In FileList
                Dim Text As List(Of String) = File.ReadAllText(TextFile).Split({vbCrLf & "}", vbLf & "}"}, StringSplitOptions.RemoveEmptyEntries).ToList 'Split the file contents into the doctrine category nested blocks.
                Text.RemoveAll(Function(x) Not x.Contains("{"c)) 'Remove any that don't seem to contain any data by searching for lack of curly brackets.

                If Text.Exists(Function(x) x.Split({vbCrLf, vbCr, vbLf}, StringSplitOptions.TrimEntries).ToList.Exists(Function(y) y.StartsWith(Doctrine))) Then
                    For Each Block In Text
                        If Block.Split({vbCrLf, vbCr, vbLf}, StringSplitOptions.TrimEntries).ToList.Exists(Function(x) x.StartsWith(Doctrine)) Then
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
            Debug.Print("Doctrine not found: " & Doctrine) 'Either there is a bug in this code or a bug in the mod.
            BuggedDoctrinesTenets.Add(Doctrine)
        End If
    End Sub
    Sub CollectLocalisations()
        Dim RawGameConceptLocalisations As New Dictionary(Of String, String)

        Dim BaseFiles As List(Of String) = Directory.GetFiles(GameDir & "\localization\english", "*.yml", SearchOption.AllDirectories).ToList
        For Each TextFile In BaseFiles
            SaveLocs(TextFile, RawGameConceptLocalisations)
        Next
        If Directory.Exists(BaseDir & "\localization\english") Then
            LocalisationFiles = Directory.GetFiles(BaseDir & "\localization\english", "*.yml", SearchOption.AllDirectories).ToList
        Else
            Console.WriteLine("Sorry, non-English localisation not currently supported. Press any key to exit.")
            Console.ReadKey()
            Exit Sub
        End If
        If Directory.Exists(BaseDir & "\localization\replace\english") Then
            LocalisationFiles = LocalisationFiles.Concat(Directory.GetFiles(BaseDir & "\localization\replace\english", "*.yml", SearchOption.AllDirectories)).ToList
        End If

        For Each Textfile In LocalisationFiles
            SaveLocs(Textfile, RawGameConceptLocalisations)
        Next

        For Each Item In RawGameConceptLocalisations.Keys
            If Not GameConceptLocalisations.Contains(Item) Then
                GameConceptLocalisations.Add(Item, DeFormat(DeComment(RawGameConceptLocalisations(Item).Split(Chr(34), 2).Last).TrimEnd.TrimEnd(Chr(34))).TrimEnd)
            Else
                GameConceptLocalisations(Item) = DeFormat(DeComment(RawGameConceptLocalisations(Item).Split(Chr(34), 2).Last).TrimEnd.TrimEnd(Chr(34))).TrimEnd
            End If
        Next
        For Count = 0 To GameConceptLocalisations.Count - 1
            If GameConceptLocalisations.Values(Count).Contains("$"c) Then
                Dim Key As String = GameConceptLocalisations.Keys(Count)
                GameConceptLocalisations(Key) = DeReference(GameConceptLocalisations.Values(Count))
            End If
        Next
    End Sub
    Private Sub GetLocalisation(ByRef Code As List(Of String), Optional Suffix As String = "")
        For Count = 0 To Code.Count - 1
            If Not Code(Count) = "" AndAlso Not Code(Count).TrimStart.StartsWith("game_concept") Then
                Dim RawCode As String = Code(Count) & Suffix 'Modify the code if the object id has a suffix in the loc code.
                If SavedLocalisation.Contains(RawCode) Then 'If the locs stored to dictionary contain this loc then...
                    Code(Count) = SavedLocalisation(RawCode)

                    'Process the loc for internal code.

                    If Code(Count).Split(Chr(34)).Last.Contains("#") Then
                        Code(Count) = DeComment(Code(Count)) 'Remove comments if any.
                    End If
                    Code(Count) = Code(Count).Split(Chr(34), 2).Last.TrimEnd.TrimEnd(Chr(34))
                    If Code(Count).Contains("#"c) Then
                        Code(Count) = DeFormat(Code(Count)) 'Remove style formatting if any.
                    End If
                    If Code(Count).Contains("|E]") OrElse Code(Count).Contains("|e]") Then
                        Code(Count) = DeConcept(Code(Count)) 'Find the appropriate locs for any game concepts referred.
                    End If
                    If Code(Count).Contains("$") Then
                        Code(Count) = DeReference(Code(Count)) 'Find the appropriate locs for any other locs referred.
                    End If
                ElseIf GameConceptLocalisations.Contains(RawCode) Then 'If game concept locs stored to dictionary contain this loc then...
                    Code(Count) = GameConceptLocalisations(RawCode) 'Get the loc from the game concept dictionary.
                Else 'If the locs stored to memory or the game concept locs stored to memory don't contain this loc then...
                    Code(Count) = RawCode 'Write down the code without any localisation.
                End If
            ElseIf Code(Count).TrimStart.StartsWith("game_concept") AndAlso GameConceptLocalisations.Contains(Code(Count).Split("game_concept_", 2).Last.Split(":"c, 2).First) Then 'If the loc starts with game_concept then look for it in the game concept dictionary.
                Code(Count) = GameConceptLocalisations(Code(Count).Split("game_concept_").Last)
            End If
        Next
    End Sub
    Sub SetGameDir()
        If BaseDir.Contains("steamapps") Then
            GameDir = BaseDir.Split("steamapps", 2).First & "steamapps\common\Crusader Kings III\game\"
        Else
            GameDirPrompt()
        End If
    End Sub
    Sub GameDirPrompt()
        Dim DirFound As Boolean = False
        Do
            Console.WriteLine("Please enter your Crusader Kings 3 installation's root directory.")
            GameDir = Console.ReadLine
            GameDir = Path.TrimEndingDirectorySeparator(GameDir)
            If Directory.GetDirectories(GameDir, SearchOption.TopDirectoryOnly).ToList.Exists(Function(x) x.Contains("binaries")) Then
                GameDir &= Path.DirectorySeparatorChar & "binaries" & Path.DirectorySeparatorChar
            End If
            If Directory.GetFiles(GameDir, SearchOption.AllDirectories).ToList.Contains(GameDir & "ck3.exe") Then
                DirFound = True
            End If
        Loop While DirFound = False

        Using SW As New StreamWriter(Path.GetTempPath() & Path.DirectorySeparatorChar & "CK3Tools.txt", False)
            SW.WriteLine(GameDir)
        End Using
    End Sub
    Sub SaveLocs(TextFile As String, RawGameConceptLocalisations As Dictionary(Of String, String))
        Using SR As New StreamReader(TextFile)
            Dim LineData As String
            While Not SR.EndOfStream
                LineData = SR.ReadLine
                If Not LineData.TrimStart.StartsWith("#"c) AndAlso LineData.Contains(":"c) AndAlso Not LineData.Split(":"c, 2).Last.Length = 0 Then

                    Dim Key As String = LineData.TrimStart.Split(":"c, 2).First
                    Dim Value As String = LineData.Split(":"c, 2).Last.Substring(1).TrimStart

                    If Not SavedLocalisation.Contains(Key) Then
                        SavedLocalisation.Add(Key, Value)
                    Else
                        SavedLocalisation(Key) = Value
                    End If
                End If
                If LineData.TrimStart.StartsWith("game_concept") Then
                    Dim Key As String = LineData.TrimStart.Split(":"c, 2).First.Split("game_concept_", 2).Last
                    Dim Value As String = LineData.Split(":"c, 2).Last.Substring(1).TrimStart

                    If Not RawGameConceptLocalisations.ContainsKey(Key) Then
                        RawGameConceptLocalisations.Add(Key, Value)
                    Else
                        RawGameConceptLocalisations(Key) = Value
                    End If
                End If
            End While
        End Using
    End Sub
    Function DeComment(Input As String) As String
        Dim Output As List(Of String) = Input.Split(Chr(34)).ToList 'Find the boundaries of the actual loc code by splitting it up according to its quotation marks.
        Output(Output.Count - 1) = Output(Output.Count - 1).Split("#"c).First 'Take the last part of the split input, and split it off from the comment.
        Return String.Join(Chr(34), Output).TrimEnd 'Rejoin the input with quotation marks and return it.
    End Function
    Function DeConcept(Input As String) As String
        If Input.Contains("]"c) AndAlso Input.Split("]"c, 2).First.Contains("["c) AndAlso Input.Split("]"c, 2).First.Split("["c, 2).Last.Contains("|"c) Then
            Dim GameConcepts As New SortedList(Of String, String) 'Collect each game concept contained in string here.
            Do While Input.Contains("]"c) AndAlso Input.Split("]"c, 2).First.Contains("["c) AndAlso Input.Split("]"c, 2).First.Split("["c, 2).Last.Contains("|"c) 'Loop while input loc string contains any non-parsed game concepts.
                Dim GameConcept As String = Input.Split("["c, 2).Last.Split("|"c, 2).First 'Get the game concept object id.
                Dim Suffix As String = "|"c & Input.Split("|"c, 2).Last.Split("]"c, 2).First & "]"c
                If Not GameConcepts.ContainsKey(GameConcept) Then 'If it has not already been collected then...
                    Dim ReplaceString As String
                    If GameConceptLocalisations.Contains(GameConcept.ToLower) Then 'Find its loc in the SortedList.
                        ReplaceString = GameConceptLocalisations(GameConcept.ToLower)
                    Else
                        ReplaceString = GameConcept 'If it cannot be found then assign the replace string to be the raw code.
                    End If
                    GameConcepts.Add(GameConcept, ReplaceString) 'Add it to the sortedlist and find the rest of the game concepts in this loc string.
                    Input = Input.Replace($"[{GameConcept}" & Suffix, ReplaceString) 'Remove it from the input string so it is not reparsed into the SortedList.
                Else 'If it has already been collected...
                    'Input = String.Concat(Input.Split({"[", "|E]"}, 3, StringSplitOptions.None)) 'Remove it from the input string.
                    Input = Input.Replace($"[{GameConcept}" & Suffix, GameConcepts(GameConcept))
                End If
            Loop

            Return Input 'Return loc.
        Else
            Return Input 'Redundancy in case a loc was falsely found to contain a game concept.
        End If

    End Function
    Function DeReference(Input As String) As String
        If Input.Contains("$"c) Then
            Dim Output As String = Input
            Dim Locs As New List(Of String)
            Do
                If Not Locs.Contains(Input.Split("$", 3)(1)) Then
                    Locs.Add(Input.Split("$", 3)(1))
                End If
                Input = Input.Split("$", 3).Last
            Loop While Input.Contains("$"c)
            Dim Code As List(Of String) = Locs.ToList
            GetLocalisation(Locs)
            For Count = 0 To Code.Count - 1
                Output = Output.Replace($"${Code(Count)}$", Locs(Count))
            Next
            Return Output
        Else
            Return Input
        End If
    End Function
    Function DeFormat(Input As String) As String
        If Input.Contains("#"c) Then
            Dim Output As String = Input
            Do
                Dim FormattedLoc As String = Output.Split("#"c, 2).Last 'Find the styled part of the loc and extract it.
                Dim DeFormatted As String
                With FormattedLoc
                    Dim CloserIndex As Integer
                    If .Contains("#!") Then
                        CloserIndex = .IndexOf("#!") + 2
                        DeFormatted = .Substring(0, CloserIndex - 2).Split(" "c, 2).Last
                    ElseIf .Contains("#"c) Then
                        CloserIndex = .IndexOf("#"c) + 1
                        DeFormatted = .Substring(0, CloserIndex - 1).Split(" "c, 2).Last
                    Else
                        CloserIndex = .Length
                        DeFormatted = FormattedLoc.Split(" "c, 2).Last
                    End If
                    FormattedLoc = "#" & .Substring(0, CloserIndex)
                End With
                'Remove the style code and store it in a string.

                Output = Output.Replace(FormattedLoc, DeFormatted) 'Use replace function to replace the formatted part of the loc with the deformatted string.
            Loop While Output.Contains("#"c) 'Loop if there are more.
            Return Output 'Return when there are no more.
        Else
            Return Input
        End If
    End Function
    Function DeNest(Input As String) As List(Of String)
        Dim Output As New List(Of String)
        Input = String.Join(vbCrLf, Input.Split({vbCrLf, vbCr, vbLf}, StringSplitOptions.None).ToList.FindAll(Function(x) Not x.TrimStart.StartsWith("#"c)))
        If Input.Contains("="c) AndAlso Input.Contains("{"c) Then
            Do
                Dim RawCodeID As String = Input.Split("{", 2).First 'Get the code id of the object the block is assigned to.
                Input = Input.Substring(RawCodeID.Length + 1) 'Split off the extracted data.
                Dim RawCodeBlock As String = Input
                RawCodeID = RawCodeID.Split({vbCrLf, vbCr, vbLf}, StringSplitOptions.None).Last
                Do While RawCodeBlock.Split("}"c, 2)(0).Contains("{"c) 'Designate subsidiary objects designated with curly brackets as such by replacing their {} with <>.
                    RawCodeBlock = String.Join(">"c, String.Join("<"c, RawCodeBlock.Split("{"c, 2)).Split("}"c, 2))
                Loop  'Loop until no more subsidiary objects.
                'End If
                RawCodeBlock = RawCodeBlock.Split("}"c)(0).Replace("<", "{").Replace(">", "}") & "}" 'Get the data of this object by splitting it off of the overall code after its own } closing bracket.
                If RawCodeID.Contains("="c) Then
                    Output.Add(String.Join("{", {RawCodeID, RawCodeBlock})) 'Add to List
                End If
                If Input.Length > RawCodeBlock.Length Then 'Split off the extracted code block.
                    Input = Input.Substring(RawCodeBlock.Length)
                Else
                    Input = ""
                End If
            Loop While Input.Split("}", 2)(0).Contains("{"c) 'Continue to parse the data until no more faiths can be found by looking for a { starting bracket.
        End If
        Return Output
    End Function
End Module