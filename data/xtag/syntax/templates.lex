; *****************************************************************
;;; Templates define feature structures in the style of PATR.
;;; Templates are organized in a hierarchical fashion using
;;; inheritance and default values.
;;;
;;; List of templates:
;;;
;;;	@1sg	1st person singular	
;;;	@2sg	2nd person singular
;;;	@3sg	3rd person singular
;;;	@2pl	2nd person plural
;;;	@pl	plural
;;;	@3pl	3rd person singular
;;;	@SG	singular
;;;	@PROG	progressive
;;;	@PAST	past tense
;;;	@PPART	past participle
;;;	@INF	infinitive
;;;	@PRES	present
;;;	@STR	strongly inflected verb
;;;	@WK	weakly inflected verb
;;;	@GEN	genitive (+ 's)
;;;	@nom	nominative case
;;;	@acc	accusative case
;;;	@nomacc	nominative or accusative case
;;;	@NEG	negation
;;;
;;; Combinations of the above templates may also appear (see the
;;; details on how templates are defined).
;;;
;;; Templates whose values are not specified but which exist in the
;;; lexicon: 
;;;
;;; 	@PASSIVE	
;;; 		only for born "V(bear) @PPART @PASSIVE @STR"
;;;
;;; 	@to
;;;   		gotta                V_Root1          "V(get) @PPART @to @STR"
;;; 	 	gonna                V_Root1          "V(go) @PROG @to"
;;; 		contracted forms
;;; 			gonna = is going to 
;;; 			gotta = got to
;;;
;;; 		other contracted forms are not present and need to be added
;;;
;;;	@COMP	
;;; 	@SUPER
;;; 		comparatives and superlatives.
;;; 
;;; Note: there may be some inconsistency between lowercase and uppercase.
;;; To do:	Make sure that 	SG -> sg
;;; 				PL -> pl
;;;
;;; SOME EXAMPLES FOLLOW:
;;;
;;; ---------------------------------------------------------------------
;;; @PROG	progressive
;;;  	ex: 	recognizer>>leaving
;;; 		  leave+ing     V(leave) @PROG
;;; ---------------------------------------------------------------------
;;; @PAST	past tense
;;; 	ex:	recognizer>>ate
;;; 		  ate     V(eat) @PAST @STR
;;; 
;;; 		recognizer>>loved
;;; 		  love+ed     V(love) @PAST @WK
;;; 		  love+ed     V(love) @PPART @WK
;;; ---------------------------------------------------------------------
;;; @PPART	past participle
;;; 	ex:	recognizer>>eaten
;;; 		  eaten     V(eat) @PPART @STR
;;; 		recognizer>>loved
;;; 		  love+ed     V(love) @PAST @WK
;;; 		  love+ed     V(love) @PPART @WK
;;; ---------------------------------------------------------------------
;;; @INF	infinitive
;;; 	ex:	recognizer>>eat
;;; 		  eat     V(eat) @INF
;;; ---------------------------------------------------------------------
;;; @PRES	present
;;; 	ex:	recognizer>>undergoes
;;; 		  undergoes     V(undergo) @3sg @PRES
;;; ---------------------------------------------------------------------
;;; @STR	strongly inflected verb
;;; 	ex:	recognizer>>ate
;;; 		  ate     V(eat) @PAST @STR
;;; 
;;; 		recognizer>>hung
;;; 		  hung     A(hung)
;;; 		  hung     V(hang) @PAST @STR
;;; 		  hung     V(hang) @PPART @STR
;;; ---------------------------------------------------------------------
;;; @WK	weakly inflected verb
;;; 	ex:	recognizer>>hanged
;;; 		  hang+ed     V(hang) @PAST @WK
;;; 		  hang+ed     V(hang) @PPART @WK
;;; ---------------------------------------------------------------------
;;; @GEN	genitive (+ 's)
;;; 	ex:	recognizer>>general's
;;; 		  general+'s     N(general) @sg @GEN
;;; ---------------------------------------------------------------------
;;; @PASSIVE only for born "V(bear) @PPART @PASSIVE @STR"
;;; 	ex:	recognizer>>born
;;; 		  born     V(bear) @PPART @PASSIVE @STR
;;; ---------------------------------------------------------------------
;;; @to	contracted forms
;;;  		gonna = is going to 
;;;  		gotta = got to
;;; 	ex:	recognizer>>gotta
;;; 		  gotta     V(get) @PPART @to @STR
;;; 
;;; 		recognizer>>gonna
;;; 		  gonna     V(go) @PROG @to
;;; 
;;;  note: other contracted forms are not present and need to be
;;;        added
;;; ---------------------------------------------------------------------
;;; @TO - infinitive forms  ('to go'  'to write')
;;; 	ex:	recognizer>> to
;;;		  to	V(to) @TO
;;; ---------------------------------------------------------------------
;;; @COMP	comparative
;;; 	ex:	recognizer>>better
;;; 		  better     N(better) @sg
;;; 		  better     A(good) @COMP
;;; 		  better     V(better) @INF
;;; 		  better     Adv(better)
;;; 
;;; 		recognizer>>happier
;;; 		  happy+er     A(happy) @COMP
;;; ---------------------------------------------------------------------
;;; @SUPER	superlative
;;; 	ex:	recognizer>>best
;;; 		  best     N(best) @sg
;;; 		  best     A(good) @SUPER
;;; 		  best     Adv(best)
;;; 
;;; 		recognizer>>happiest
;;; 		  happy+est     A(happy) @SUPER
;;; ---------------------------------------------------------------------
;;; @1sg	1st person singular	
;;;	ex:	recognizer>>i
;;;		  i     N(i) @sg
;;;		  i     Pron(I) @1sg @nom
;;; ---------------------------------------------------------------------
;;; @2sg	2nd person singular
;;; ---------------------------------------------------------------------
;;; @3sg	3rd person singular
;;;	ex:	recognizer>>him
;;;		  him     Pron(him) @acc @3sg @masc
;;; ---------------------------------------------------------------------
;;; @2pl	2nd person plural;;; ---------------------------------------------------------------------
;;; @pl		plural
;;; 	ex:	recognizer>>men
;;; 		  men     N(man) @pl
;;; ---------------------------------------------------------------------
;;; @3pl	3rd person singular
;;;	ex:	recognizer>>they
;;;		  they     Pron(they) @nom @3pl
;;; ---------------------------------------------------------------------
;;; @sg		singular
;;; 	ex:	recognizer>>man
;;; 		  man     N(man) @sg
;;; 		  man     V(man) @INF
;;; ---------------------------------------------------------------------
;;; @masc 	masculine
;;;	ex:	recognizer>>him
;;;		  him     Pron(him) @acc @3sg @masc
;;; ---------------------------------------------------------------------
;;; @fem 	feminine
;;;	ex:	  her     Pron(her) @acc @3sg @fem
;;;		  her     D(her)
;;; ---------------------------------------------------------------------
;;; @nom 	nominative
;;;	ex:	recognizer>>he
;;;		  he     N(he) @sg
;;;		  he     Pron(he) @nom @3sg @masc
;;; ---------------------------------------------------------------------
;;; @acc 	accusative
;;;	ex:	recognizer>>him
;;;		  him     Pron(him) @acc @3sg @masc
;;; ---------------------------------------------------------------------
;;; @nomacc 	nominative or accusative
;;;	ex: 	recognizer>>nobody
;;;		  nobody     N(nobody) @sg
;;;		  nobody     Pron(nobody) @3sg @nomacc @NEG
;;; ---------------------------------------------------------------------
;;; @NEG 	negation
;;;	ex:	recognizer>>nobody
;;;		  nobody     N(nobody) @sg
;;;		  nobody     Pron(nobody) @3sg @nomacc @NEG
;;;
;;; *****************************************************************

;;; Definition of templates:

;;; person
@1st	<agr pers> = 1!
@2nd	<agr pers> = 2!
@3rd	<agr pers> = 3!

;;; number
@sg	<agr num> = sing!
@pl	<agr num> = plur!

;;; gender
@fem	<agr gen> = fem!
@masc	<agr gen> = masc!
@neut	<agr gen> = neuter!


;;; genitive/possessive
@GEN	<gen> = +!
@GEN-	<gen> = -!

;;; case
@nom	<case> = nom, @GEN- !
@acc	<case> = acc, @GEN- !
@nomacc	<case> = nom/acc, @GEN- !

;;; case from prepositions
@p_acc	<assign-case> = acc!

;;; combinations of person and number
@1sg	@1st, @sg, <agr 3rdsing> = -!
@2sg	@2nd, @sg, <agr 3rdsing> = -!
@3sg	@3rd, @sg, <agr 3rdsing> = +!

@1pl	@1st, @pl, <agr 3rdsing> = -!
@2pl	@2nd, @pl, <agr 3rdsing> = -!
@3pl	@3rd, @pl, <agr 3rdsing> = -!

;; hack!!
@not-3sg   <agr pers> = 1/2/3, <agr num> = plur/sing, <agr 3rdsing> = -!

;;; for pronoun references
;;; 6/20/95: These features are only used by the genitive pronouns.
;;; It is not at all evident what the features do, but removing them would 
;;; entail digging around in the morphology code. Since the features appear 
;;; to be harmless, we are not removing them at present. CDD
@ref1st	<ref pers> = @1st!
@ref2nd	<ref pers> = @2nd!
@ref3rd	<ref pers> = @3rd!

@refsg	<ref num> = @sg!
@refpl	<ref num> = @pl!

@ref1sg	@ref1st, @refsg!
@ref2sg	@ref2nd, @refsg!
@ref3sg	@ref3rd, @refsg!

@ref1pl	@ref1st, @refpl!
@ref2pl	@ref2nd, @refpl!
@ref3pl	@ref3rd, @refpl!

@reffem	<ref gen> = @fem!
@refmasc <ref gen> = @masc!

;;;feature used by reflexive pronouns
@refl	<refl> = +!
;;;feature used by all other nouns
@refl-	<refl> = -!

;;; assign subject case 
@NOM-ASSIGN <assign-case>=nom!
@NOM-ACC-ASSIGN <assign-case>=nom/acc!
@ACC-NONE-ASSIGN <assign-case>=acc/none!
@NONE-ASSIGN <assign-case>=none!

@COMP-IND-ASSIGN <assign-comp> = ind_nil/that/if/whether!

;;; verb modes
@INF	<mode> = ind/base!
@IND	<mode> = ind, <tense> = pres, @COMP-IND-ASSIGN, @NOM-ASSIGN, @not-3sg !
@BASE	<mode> = base!
@SBJNCT	<mode> = sbjnct, <mainv> = -, @NOM-ASSIGN !
@PAST	<mode> = ind, <tense> = past, @COMP-IND-ASSIGN, @NOM-ASSIGN !
@PPART	<mode> = ppart!
@PROG	<mode> = ger!
@PRES	<mode> = ind, <tense> = pres, @COMP-IND-ASSIGN, @NOM-ASSIGN !
@INDAUX	<mode> = ind, <mainv> = -, @COMP-IND-ASSIGN, @NOM-ASSIGN !
@TO	<mode> = inf, <mainv> = -!

;;; weak or strong inflection of verbs
@WK	<weak> = +!
@STR	<weak> = -!

;;; mark for negation
@NEG	<neg> = +!

;;; no wh 
;;; wh- is a default
@WH-	<wh> = -!

;;; wh 
@WH+		<wh> = +!
@WH		<wh> = +!

;;;mark as pronoun
@PRON+		<pron> = +!


;; contraction -- 6/14/94 think this also needs mainv=- feature
@contr	<contr> = +!

;; comp - comparatives
@comp <compar> = +, <equiv> = -!

;; super - superlatives
@super <super> = +!

;;;Determiner features used in NP default entry, in "translate."
@card-	<card>=-!
@const-	<const>=-!
@decreas-	<decreas>=-!
@definite-	<definite>=-!
@definite+	<definite>=+!
@quan-	<quan>=-!

#A_WH+		A.b:<wh>=+!
#A_WH-		A.b:<wh>=-!
#P_WH+		P.b:<wh>=+!
#P_WH-          P.b:<wh>=-!
#N_PRON		N.b:<pron>=+!

;;; the following is used for the auxiliary verbs
#V_ind		V.b:<mode>=ind!
#V_base		V.b:<mode>=base!
#V_ppart	V.b:<mode>=ppart!
#V_passive-     V.b:<passive>=-!
#V_ind_ger	V.b:<mode>=ind/ger!
#V_ind_base_ppart_sbjnct      V.b:<mode>=ind/base/ppart/sbjnct!
#V_ger          V.b:<mode>=ger!
#V_mainv-	V.b:<mainv> =-!
#V_mainv+	V.b:<mainv> =+!
#V_nom		V.b:<assign-case>=nom!
#V_pres		V.b:<tense>=pres!
#V_sbj_control  S_2.t:<control> = NP_0.t:<control>!
#V_obj_control  S_2.t:<control> = NP_1.t:<control>!
#S_nomprep_aux	S.b:<mode>=nom/prep!
#S_inf		S.b:<mode>=inf!
#S_ppart	S.b:<mode>=ppart!
#S_ppart2	S:<mode>=ppart!
#S_ger-inf      S:<mode>=ger/inf!
#S_mainv+	S.b:<mainv>=+!
#S_pass+	S.b:<passive>=+!
#S_pass-	S.b:<passive>=-!
#S_pass2+	S:<passive>=+!
#S_ger		S.b:<mode>=ger!
#S_base		S.b:<mode>=base!
#S_imp	        S.b:<mode>=imp!
#S_extr-	S.b:<extracted>=-!
#S1_extr-	S_1.t:<extracted>=-!
#S2_extr-	S_2.t:<extracted>=-!
#S_assign-comp-inf_nil	S:<assign-comp>=inf_nil!
#Sr_prog+	S_r.b:<progressive>=+!
#Sr_perfect+	S_r.b:<perfect>=+!
#VP_nomprep	VP.b:<mode>=nom/prep!
#VP_inf		VP.b:<mode>=inf!
#VP_infnomprep	VP.b:<mode>=inf/nom/prep!
#VP_ppart	VP.b:<mode>=ppart!
#VP_mainv+	VP.b:<mainv>=+!
#VP_pass+	VP.b:<passive>=+!
#VP_pass-	VP.b:<passive>=-!
#VP_ger		VP.b:<mode>=ger!
#VP_base	VP.b:<mode>=base!
#VP_infnom	VP.b:<mode>=inf/nom!
#VPr_mainv+	VP_r.b:<mainv>=+!
#VPr_mainv-	VP_r.b:<mainv>=-!
#VPr_prog+	VP_r.b:<progressive>=+!
#VPr_perfect+	VP_r.b:<perfect>=+!

;; ************* Features for sentential complements ***************
;; Complementizers can only appear with certain types of S-complements
;; A complementizer can never adjoin to a question -- hence the 
;; feature S_r.t:<wh> = -. On "for", the extracted=- feature blocks it
;; from adjoining above the subject of topicalized setences (*want Mary
;; PRO for to...) but does not interfere with "for" adjoining into
;; long-distance extractions (who does Bill want for Mary to meet).

#COMP_THAT	Comp.b:<comp>=that, S_r.t:<wh> = -, S_r.b:<assign-comp>=that!
#COMP_WHETHER	Comp.b:<comp>=whether, S_r.t:<wh> = -,  S_r.b:<assign-comp>=whether!
#COMP_IF	Comp.b:<comp>=if, S_r.t:<wh> = -, S_r.b:<assign-comp>=if!
#COMP_FOR	Comp.b:<comp>=for, S_r.t:<wh> = -, S_r.b:<assign-comp>=for, S_r.b:<assign-case>=acc, S_r.t:<extracted> = -!

;; "if" and "whether" are wh+, other comps are wh-
#COMP_WH+	Comp.b:<wh>=+!
#COMP_WH-	Comp.b:<wh>=-!
;;types of complements allowed by the complementizers
;; "that"
#Sc_ind_subjn S_c.b:<mode>=ind/sbjnct!
;; "whether"
#Sc_ind_inf   S_c.b:<mode>=ind/inf!
;; "if"
#Sc_ind               S_c.b:<mode>=ind!
;; "for"
#Sc_inf               S_c.b:<mode>=inf!

;; these features are used by infinitival "to" 
#V_assign-comp_nil_whether	V.b:<assign-comp>= whether/inf_nil!
;; "to" for "for-to" construction and ECM; does not assign any case at all
#V_assign-comp_for	V.b:<assign-comp>=for/ecm!
;; this is used by passive "be"
#V_assign-comp_none	V.b:<assign-comp>=none!

;; The following templates provide more specific information on the verbs than
;;  the previous S1_INDIC and S1_INFIN templates. 
#S1_base_nil           S_1.t:<mode>=base, S_1.t:<comp>=nil!
#S1_ind-sbjnct_that-nil S_1.t:<mode>=ind/sbjnct, S_1.t:<comp>=that/nil!
#S1_ind-sbjnct         S_1.t:<mode>=ind/sbjnct, S_1.t:<comp>=whether/if/that/nil!
#S1_ind_that-nil       S_1.t:<mode>=ind, S_1.t:<comp>=that/nil!
#S1_ind_that           S_1.t:<mode>=ind, S_1.t:<comp>=that!
#S1_ind 	       S_1.t:<mode> = ind,S_1.t:<comp>= that/whether/if/nil!
#S1_inf                S_1.t:<mode> = inf,S_1.t:<comp> = whether/for/nil!
#S1_inf_for_nil        S_1.t:<mode>=inf, S_1.t:<comp>=for/nil!
#S1_inf_nil            S_1.t:<mode>=inf, S_1.t:<comp>=nil!
#S1_inf_whether_nil    S_1.t:<mode>=inf, S_1.t:<comp>=whether/nil!
#S1_ind-inf_whether-if S_1.t:<mode>=ind/inf, S_1.t:<comp>=whether/if!
#S1_wonder1 S_1.t:<mode>=ind/inf, S_1.t:<comp>=whether/if!
#S1_wonder2 S_1.t:<mode>=ind/inf, S_1.t:<comp>=nil, S_1.t:<extracted>=+ !
#S1_ECM_acc            S_1.t:<assign-case>=acc!
#S1_sbjnct         S_1.t:<mode>=sbjnct, S_1.t:<comp>=that!

;; The following templates provide more specific information on the verbs 
;; than the previous S2_INDIC and S2_INFIN templates.  These templates may 
;; also be used by other parts of speech, such as adjectives.
#S2_inf_nil            S_2.t:<mode>=inf, S_2.t:<comp>=nil!
#S2_inf_whether-nil            S_2.t:<mode>=inf, S_2.t:<comp>=whether/nil!
#S2_inf_for-nil        S_2.t:<mode>=inf, S_2.t:<comp>=for/nil!
#S2_ind 	S_2.t:<mode> = ind,S_2.t:<comp>= that/whether/if/nil!
#S2_ind_that-nil S_2.t:<mode>=ind, S_2.t:<comp>=that/nil!

;;;;WH features on sentential complements
#S1_WH+		S_1.t:<wh> = +!
#S2_WH+		S_2.t:<wh> = +!
#S1_WH-		S_1.t:<wh> = -!
#S2_WH-		S_2.t:<wh> = -!

#S1_NOMPREP	S_1.t:<mode> = nom/prep, S_1.t:<comp>=nil, S_1.t:<assign-case> = acc!
#S1_NOMPREPBASE	S_1.t:<mode> = nom/prep/base, S_1.t:<comp>=nil, S_1.t:<assign-case> = acc!
#S1_PREP	S_1.t:<mode> = prep, S_1.t:<comp>=nil, S_1.t:<assign-case> = acc!

;; these features are used by NOUNs taking S complements, and
;; subordinating conjunctions
#THAT_S_COMP	S.t:<mode>=ind, S.t:<comp>=that!
#NIL_S_COMP	S.t:<mode>=ind, S.t:<comp>=nil!
#THAT-NIL_S_COMP	S.t:<mode>=ind, S.t:<comp>=that/nil!
#INF_S_COMP	S.t:<mode>=inf, S.t:<comp>=nil!
#INF-FOR_S_COMP	S.t:<mode>=inf, S.t:<comp>=for/nil!
#SBJNCT-NIL_S_COMP  S.t:<mode>=sbjnct/ind, S.t:<comp>=nil!
#SBJNCT-IND_S_COMP  S.t:<mode>=sbjnct/ind, S.t:<comp>=nil!
#PPART_S_COMP  S.t:<mode>=ppart, S.t:<comp>=nil!
#PPART-GER_S_COMP  S.t:<mode>=ppart/ger, S.t:<comp>=nil!
#PRED_S_COMP	S.t:<mode>=nom/prep, S.t:<comp>=nil!
#INF_S1_COMP	S_1.t:<mode>=inf, S_1.t:<comp>=nil!


;;****************************************************************

#N_plur		N.b:<agr num>=plur!
#N_sing		N.b:<agr num>=sing!
#N_wh-		N.b:<wh>=-!
#N1_plur	N_1.t:<agr num>=plur!
#N1_sing	N_1.t:<agr num>=sing!
#S_3rdsing+	S.b:<agr 3rdsing>=+!
#S_sing		S.b:<agr num>=sing!
#S_pers3	S.b:<agr pers>=3!
#S_cond-	S.b:<conditional>=-!
#S_perfect-	S.b:<perfect>=-!
#S_prog-	S.b:<progressive>=-!
#S_comp_for-nil	S:<comp>=for/nil!
#Sr_ind		S_r.b:<mode>=ind!
#Sr_past	S_r.b:<tense>=past!
#Sr_pres	S_r.b:<tense>=pres!
#Sr_irrealis+   S_r.b:<irrealis>=+!
#VP_cond-	VP.b:<conditional>=-!
#VPt_neg+	VP.t:<neg>=+!
#VP_perfect-	VP.b:<perfect>=-!
#VP_prog-	VP.b:<progressive>=-!
#VPr_base	VP_r.b:<mode>=base!
#VPr_ger	VP_r.b:<mode>=ger!
#VPr_ind	VP_r.b:<mode>=ind!
#VPr_ind_long	VP_r.t:<mode>=ind!
#VPr_past	VP_r.b:<tense>=past!
#VPr_present	VP_r.b:<tense>=pres!
#VPr_pres_long	VP_r.t:<tense>=pres!
#VPr_3rdsing+	VP_r.t:<agr 3rdsing>=+!
#VPr_sing	VP_r.t:<agr num>=sing!
#VPr_pers3	VP_r.t:<agr pers>=3!
#V_inf		V.b:<mode>=inf!
#V_eps_extra	VP_r.b:<mode>=VP.t:<mode>, VP_r.b:<passive>=VP.t:<passive>, VP_r.b:<agr>=VP.t:<agr>, VP_r.b:<mainv>=VP.t:<mainv>!

#N_refl-	N.b:<refl>=-!
#NP_refl-	NP.b:<refl>=-! 	;used for PRO
#NP_plur	NP.b:<agr num>=plur!
#REFL_OBJ	V.b:<refl>=+!

#D1_card+	D_1.b:<card>=+!
#D2_card+	D_2:<card>=+!
#D_3sg		D.b:<agr num>=sing, D.b:<agr pers>=3, D.b:<agr 3rdsing>=+!
#D_3pl		D.b:<agr num>=plur, D.b:<agr pers>=3, D.b:<agr 3rdsing>=-!
#D_3	        D.b:<agr pers>=3!

#D_wh-		D.b:<wh>=-!
#D_wh+		D.b:<wh>=+!
#D_definite-	D.b:<definite>=-!
#D_definite+	D.b:<definite>=+!
#D_quan-	D.b:<quan>=-!
#D_quan+	D.b:<quan>=+!
#D_card-	D.b:<card>=-!
#D_card+	D.b:<card>=+!
#D_gen-		D.b:<gen>=-!
#D_gen+		D.b:<gen>=+!
#D_decreas-	D.b:<decreas>=-!
#D_decreas+	D.b:<decreas>=+!
#D_const-	D.b:<const>=-!
#D_const+	D.b:<const>=+!
#D_compl+      D.b:<compl>=+!
#D_compl-      D.b:<compl>=-!


;;New determiner/np analysis
#NXf_definite-	NP_f.t:<definite>=-!
#NXf_definite+	NP_f.t:<definite>=+!
#NXf_quan+	NP_f.t:<quan>=+!
#NXf_quan-	NP_f.t:<quan>=-!
#NXf_card+	NP_f.t:<card>=+!
#NXf_card-	NP_f.t:<card>=-!
#NXf_gen-	NP_f.t:<gen>=-!
#NXf_decreas+	NP_f.t:<decreas>=+!
#NXf_decreas-	NP_f.t:<decreas>=-!
#NXf_const+	NP_f.t:<const>=+!
#NXf_const-	NP_f.t:<const>=-!
#NXf_3sg 	NP_f.t:<agr num>=sing, NP_f.t:<agr pers>=3, NP_f.t:<agr 3rdsing>=+!
#NXf_wh-	NP_f.t:<wh>=-!
#NXf_compl+     NP_f.t:<compl>=+!
#NXf_compl-     NP_f.t:<compl>=-!
#NXf_3          NP_f.t:<agr pers>=3!
#NXf_3pl 	NP_f.t:<agr num>=plur, NP_f.t:<agr pers>=3, NP_f.t:<agr 3rdsing>=-!

#D_AGR_NXr	D.t:<agr>=NP_r.b:<agr>!
#Df_card+	D_f.t:<card>=+!
#Df_const-	D_f.t:<const>=-!

#N_definite+	N.b:<definite>=+!
#N_definite-	N.b:<definite>=-!
#N_quan-	N.b:<quan>=-!
#N_card-	N.b:<card>=-!
#N_gen-		N.b:<gen>=-!
#N_gen+		N.b:<gen>=+!
#N_decreas-	N.b:<decreas>=-!
#N_const-	N.b:<const>=-!
#N_const+       N.b:<const> = +!

;; These are used for restrictions on the predicational NP in light verbs
#NP1_const+     NP_1.t:<const> = +!
#NP1_gen+       NP_1.t:<gen> = +!
#NP1_def-       NP_1.t:<definite> = -!
#NP1_compl+     NP_1.t:<compl> = +!
 
#G_WH+		G.b:<wh>=+!
#G_WH-		G.b:<wh>=-!

;;;Gave these all separate values in case we decided they behave 
;;;differently somewhere; possibly collapsible into comma/non-comma 
;;;distinction 
#Comma_conj	Conj.b:<conj>=comma!
#Scolon_conj	Conj.b:<conj>=scolon!
#And_conj	Conj.b:<conj>=and!
#But_conj	Conj.b:<conj>=but!
#Or_conj	Conj.b:<conj>=or!
#To_conj	Conj.b:<conj>=to!
#Comma_conj1	Conj_1.b:<conj>=comma!
#Scolon_conj1	Conj_1.b:<conj>=scolon!
#And_conj1	Conj_1.b:<conj>=and!
#But_conj1	Conj_1.b:<conj>=but!
#Or_conj1	Conj_1.b:<conj>=or!

;;;Features to block things conjoined by commas from being themselves 
;;;conjoined by commas, i.e. *(Mary, John), (Bill, Sue)
#NP_comma_conj	NP_2.t:<conj>=nil, NP_1.t:<conj>=comma/nil!
#N_comma_conj	N_2.t:<conj>=nil, N_1.t:<conj>=comma/nil!
#AP_comma_conj	AP_2.t:<conj>=nil, AP_1.t:<conj>=comma/nil!
#A_comma_conj	A_2.t:<conj>=nil, A_1.t:<conj>=comma/nil!
#Ad_comma_conj	Ad_2.t:<conj>=nil, Ad_1.t:<conj>=comma/nil!
#PP_comma_conj	PP_2.t:<conj>=nil, PP_1.t:<conj>=comma/nil!
#P_comma_conj	P_2.t:<conj>=nil, P_1.t:<conj>=comma/nil!
#Det_comma_conj D_2.t:<conj>=nil, D_1.t:<conj>=comma/nil!
#NP_scolon_conj	NP_2.t:<conj>=nil, NP_1.t:<conj>=scolon/nil!
#PP_scolon_conj	PP_2.t:<conj>=nil, PP_1.t:<conj>=scolon/nil!
#A_scolon_conj A_2.t:<conj>=nil, A_1.t:<conj>=scolon/nil!

;;; Features on lexical conj trees, to block things conjoined by commas from
;;; substituting as the right conjunct. Also, right conjunct cannot be conjoined by semicolon
#NP_lex_conj	NP_2.t:<conj>=and/or/but/nil! ;;;, NP_1.t:<conj>=and/or/but/nil/comma!
#N_lex_conj	NP_2.t:<conj>=and/or/but/nil! ;;;  N_1.t:<conj>=and/or/but/nil/comma!
#A_lex_conj	A_2.t:<conj>=and/or/but/nil!
#PP_lex_conj	PP_2.t:<conj>=and/or/but/nil!
#Ad_lex_conj	Ad_2.t:<conj>=and/or/but/nil!
#S_lex_conj	S_2.t:<conj>=and/or/but/nil!
#Det_lex_conj	D_2.t:<conj>=and/or/but/nil!
#AP_lex_conj	AP_2.t:<conj>=and/or/but/nil!
#P_lex_conj	P_2.t:<conj>=and/or/but/nil!

;;;;Agreement features for NP coordination
#Or_NPagr	NP.b:<agr num> = NP_2:<agr num>! 
#Or_Nagr 	N.b:<agr num> = N_2:<agr num>!
#But_NPagr 	NP.b:<agr num> = NP_1:<agr num>!
#But_Nagr 	N.b:<agr num> = N_1:<agr num>!
#Not_NP1agr 	NP.b:<agr num> = NP_1:<agr num>!
#Not_NP2agr 	NP.b:<agr num> = NP_2:<agr num>!

;;;;Feats for punctuation
#Comma	Punct.b:<punct struct>=comma!
#Comma1	Punct_1.b:<punct struct>=comma!
#Dash	Punct.b:<punct struct>=dash, Punct.b:<punct contains dash>=+!
#Dash1	Punct_1.b:<punct struct>=dash, Punct_1.b:<punct contains dash>=+!
#Paren	Punct_1.b:<punct bal>=paren!
#DQuote	Punct_1.b:<punct bal>=dquote!
#SQuote	Punct_1.b:<punct bal>=squote!
#Period	Punct.b:<punct term>=per, S_f:<mode>=ind/imp!
#Qmark	Punct.b:<punct term>=qmark, S_f:<mode>=ind/imp!
#Excl	Punct.b:<punct term>=excl, S_f:<mode>=ind/imp!
#Semicolon	Punct.b:<punct struct>=scolon, Punct.b:<punct contains scolon>=+!
#Colon	Punct.b:<punct struct>=colon, Punct.b:<punct contains colon>=+!

#Sf_punct-	S_f.t:<punct struct>=nil!
#Sf_term_none	S_f.t:<punct term>=nil!
#Sf_term_qmark	S_f.t:<punct term>=qmark!
#Sf_term_excl	S_f.t:<punct term>=excl!
#Sf_term_excl-qmark	S_f.t:<punct term>=excl/qmark!
#Sf_ppart-ger-inf	S_f:<mode>=ppart/ger/inf!
#S_no_colon	S_1.t:<punct contains colon>= -, S_f.t:<punct contains colon>= -!
#S_no_scolon	S_1.t:<punct contains scolon>= -, S_f.t:<punct contains scolon>= -!
#S1_ind-imp-inf	S_1.t:<mode> = ind/imp/inf!
#S1_ind-imp	S_1.t:<mode> = ind/imp!
#S1_ind 	S_1.t:<mode> = ind!
#NP_no_colon	NP.t:<punct contains colon> = -!
#NP_PRON-	NP.t:<pron>=-!	;;used by appositive tree
#NP_conj1	NP.t:<conj> = and/or/but/nil!
#NP_conj2	NP.t:<conj> = and/or/but/nil/comma!

#Dquote_np	NP_f.t:<punct bal> = squote/nil, NP_r.b:<punct contains dquote> = +, NP_f.t:<punct contains dquote> = - !
#Dquote_s	S_f.t:<punct struct> = nil, S_r.b:<punct contains dquote> = +, S_f.t:<punct contains dquote> = - !
#Dquote_n	N_f.t:<punct bal> = squote/nil, N_r.b:<punct contains dquote> = +, N_f.t:<punct contains dquote> = - !
#Dquote_adv	Ad_f.t:<punct bal> = squote/nil, Ad_r.b:<punct contains dquote> = +, Ad_f.t:<punct contains dquote> = - !
#Dquote_adj	A_f.t:<punct bal> = squote/nil, A_r.b:<punct contains dquote> = +, A_f.t:<punct contains dquote> = - !
#Dquote_det	D_f.t:<punct bal> = squote/nil, D_r.b:<punct contains dquote> = +, D_f.t:<punct contains dquote> = - !
#Dquote_vp      VP_f.t:<punct bal> = squote/nil, VP_r.b:<punct contains dquote> = +, VP_f.t:<punct contains dquote> = - ! 
#Dquote_v      V_f.t:<punct bal> = squote/nil, V_r.b:<punct contains dquote> = +, V_f.t:<punct contains dquote> = - ! 
#Dquote_pp     PP_f.t:<punct bal> = squote/nil, PP_r.b:<punct contains dquote> = +, PP_f.t:<punct contains dquote> = - !
#Dquote_p      P_f.t:<punct bal> = squote/nil, P_r.b:<punct contains dquote> = +, P_f.t:<punct contains dquote> = - !

#Squote_s	S_f.t:<punct struct> = nil, S_r.b:<punct contains squote> = +, S_f.t:<punct contains squote> = - !
#Squote_np	NP_f.t:<punct bal> = dquote/nil, NP_r.b:<punct contains squote> = +, NP_f.t:<punct contains squote> = - !
#Squote_n	N_f.t:<punct bal> = dquote/nil, N_r.b:<punct contains squote> = +, N_f.t:<punct contains squote> = - !
#Squote_adv	Ad_f.t:<punct bal> = dquote/nil, Ad_r.b:<punct contains squote> = +, Ad_f.t:<punct contains squote> = - !
#Squote_adj	A_f.t:<punct bal> = dquote/nil, A_r.b:<punct contains squote> = +, A_f.t:<punct contains squote> = - !
#Squote_det	D_f.t:<punct bal> = dquote/nil, D_r.b:<punct contains squote> = +, D_f.t:<punct contains squote> = - !
#Squote_vp      VP_f.t:<punct bal> = dquote/nil, VP_r.b:<punct contains squote> = +, VP_f.t:<punct contains squote> = - ! 
#Squote_v      V_f.t:<punct bal> = dquote/nil, V_r.b:<punct contains squote> = +, V_f.t:<punct contains squote> = - ! 
#Squote_pp     PP_f.t:<punct bal> = dquote/nil, PP_r.b:<punct contains squote> = +, PP_f.t:<punct contains squote> = - !
#Squote_p      P_f.t:<punct bal> = dquote/nil, P_r.b:<punct contains squote> = +, P_f.t:<punct contains squote> = - !

;; feature which forces prepositions to assign accusative case
#p_acc	P.b:<assign-case> = acc!
#p1_acc	P_1.b:<assign-case> = acc!

;; features for comparatives
#Ad_equiv+      Ad.b:<equiv>=+!
#Ad_equiv-      Ad.b:<equiv>=-!
#P_equiv+       P.b:<equiv>=+!
#P_equiv-       P.b:<equiv>=-!
#Ad_compar+     Ad.b:<compar>=+!
#Ad_compar-     Ad.b:<compar>=-!
#P_compar+      P.b:<compar>=+!
#P_compar-      P.b:<compar>=-!
#A_compar-      A.b:<compar>=-!

;; features for control
#Vs1_control  S_1.t:<control> = NP_0.t:<control>!


;; topmost features when start label is S
#S_TOPFEATS     Dn.t:<mode>=ind/imp, Dn.t:<comp>=nil, Dn.t:<wh>=Dn.t:<invlink>, Dn.t:<punct term>=per/qmark/excl, Dn.t:<punct struct>=nil!


