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
    Dim GroupOfCategory, CategoryOfDoctrine As New Dictionary(Of Integer, Integer) 'These Dictionaries have the Category and the Doctrine as keys, with the values being their parent.
    Dim Tenets As New List(Of String) 'These will be the raw id of tenets.
    Dim Blocks As New List(Of String) 'Store non-parsed data from files that contain religion data.
    Dim BuggedDoctrinesTenets As New List(Of String) 'This will store any doctrines or tenets that were not found in the game files allowing skipping over these in code.
#Enable Warning IDE0044 ' Add readonly modifier
    Sub Main()

        Dim FileList As List(Of String) = Directory.GetFiles(BaseDir & "\common\religion\religions", "*.txt", SearchOption.AllDirectories).ToList 'Get all doctrine code files.

        Dim Religions, Faiths As New List(Of String) 'Raw ids of religion that will later be named.
        Dim ReligionDoctrines, ReligionFaiths, FaithDoctrines As New Dictionary(Of Integer, String) 'These dictionaries will contain doctrines and faiths assigned according to the index of the prior lists.
        Dim ReligionOfFaith As New Dictionary(Of Integer, Integer) 'This dictionary will have faiths as the keys and parent religion as value.

        For Each TextFile In FileList
            Dim Text As List(Of String) = File.ReadAllText(TextFile).Split(vbCrLf & "}", StringSplitOptions.RemoveEmptyEntries).ToList 'Split the file contents into the religion nested blocks.

            Text.RemoveAll(Function(x) Not x.Contains("{"c) OrElse x.StartsWith("#"c)) 'Remove any that don't seem to contain any data by searching for lack of curly brackets.

            For Each Block In Text
                Dim Religion As String = Block.Split({"="c, "{"c}, 2)(0).Trim.Split({vbCrLf, vbTab, " "c}, StringSplitOptions.None).Last 'Get the raw id of the religion.

                Religions.Add(Religion) 'Add the religion then get the index.
                Religion = Religions.Count - 1

                Dim RawDoctrines As List(Of String) = Block.Split("faiths", 2)(0).Split(vbCrLf, StringSplitOptions.TrimEntries).ToList.FindAll(Function(x) x.StartsWith("doctrine") AndAlso x.Contains("="c) AndAlso Not x.StartsWith("doctrine_")) 'Get the doctrines of the religion but not child faiths.

                For Each Doctrine In RawDoctrines
                    Doctrine = Doctrine.Split("="c, 2)(1).Trim.Split(" "c).First
                    If Not BuggedDoctrinesTenets.Contains(Doctrine) Then
                        If Doctrines.Contains(Doctrine) Then 'Make sure doctrine exists in database. Doctrines are added on-demand. Then get index of doctrine.
                            Doctrine = Doctrines.IndexOf(Doctrine)
                        Else
                            AddDoctrine(Doctrine)
                            If Doctrines.Contains(Doctrine) Then
                                Doctrine = Doctrines.Count - 1
                            Else
                                Doctrine = -1 '-1 signifies that the doctrine does not exist in game code.
                            End If
                        End If

                        If Not Doctrine = -1 Then
                            If Not ReligionDoctrines.ContainsKey(Religion) Then
                                ReligionDoctrines.Add(Religion, Doctrine) 'Add the indexes of every doctrine of religion into the dictionary.
                            Else
                                ReligionDoctrines(Religion) &= $" {Doctrine}"
                            End If
                        End If
                    End If
                Next

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
                            If Doctrines.Contains(Doctrine) Then 'Make sure doctrine exists in database. Doctrines are added on-demand. Then get index of doctrine.
                                Doctrine = Doctrines.IndexOf(Doctrine)
                            Else
                                AddDoctrine(Doctrine)
                                If Doctrines.Contains(Doctrine) Then
                                    Doctrine = Doctrines.Count - 1
                                Else
                                    Doctrine = -1 '-1 signifies that the doctrine does not exist in game code.
                                End If
                            End If

                            If Not Doctrine = -1 Then
                                If Not FaithDoctrines.ContainsKey(FaithIndex) Then
                                    FaithDoctrines.Add(FaithIndex, Doctrine) 'Add the indexes of every doctrine of religion into the dictionary.
                                Else
                                    FaithDoctrines(FaithIndex) &= $" {Doctrine}"
                                End If
                            End If
                        End If
                    Next
                Next
            Next
        Next
        Stop
    End Sub
    Sub AddDoctrine(Doctrine As String)
        Dim FileList As List(Of String) = Directory.GetFiles(BaseDir & "\common\religion\doctrines", "*.txt", SearchOption.AllDirectories).ToList 'Get all doctrine code files.

        Dim TextBlock As String = "" 'The string to which the block containing relevant data will be assigned to.

        If Blocks.Exists(Function(x) x.Split(vbCrLf, StringSplitOptions.TrimEntries).ToList.Exists(Function(y) y.StartsWith(Doctrine))) Then 'Search for the data in already accessed game data first.
            TextBlock = Blocks.Find(Function(x) x.Split(vbCrLf, StringSplitOptions.TrimEntries).ToList.Exists(Function(y) y.StartsWith(Doctrine)))
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
End Module