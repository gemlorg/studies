; ModuleID = 'program'


 


declare external ccc  void @printInt(i32)    


declare external ccc  void @printString(i8*)    


declare external ccc  void @error()    


declare external ccc  i32 @readInt()    


declare external ccc  i8* @readString()    


declare external ccc  i8* @_concatStrings(i8*, i8*)    


declare external ccc  i32 @_compareStrings(i8*, i8*)    


declare external ccc  i8* @_malloc(i32)    


declare external ccc  i32 @_count_arr_length(i8*, i32)    


%_arr = type {i8*, i32}


define external ccc  %_arr* @__arr_malloc()    {
entry:
  %0 = getelementptr inbounds %_arr, %_arr* zeroinitializer, i32 1 
  %1 = ptrtoint %_arr* %0 to i32 
  %2 =  call ccc  i8*  @_malloc(i32  %1)  
  %3 = bitcast i8* %2 to %_arr* 
  ret %_arr* %3 
}


define external ccc  i32 @d()    {
entry:
  ret i32 0 
}


define external ccc  i32 @s(i32  %x)    {
entry:
  %_v0 = add   i32 %x, 1 
  ret i32 %_v0 
}


define external ccc  i32 @main()    {
entry:
  br label %IB_1 
IB_1:
  br label %IB_2 
IB_2:
  %_v0 = bitcast i32 0 to i32 
  br label %IB_3 
IB_3:
  %_iv1 = add   i32 %_v0, 1 
  br label %IB_4 
IB_4:
  %_v1 = bitcast i32 %_iv1 to i32 
  br label %IB_5 
IB_5:
  %_iv2 = add   i32 %_v1, 1 
  br label %IB_6 
IB_6:
  %_v2 = bitcast i32 %_iv2 to i32 
  br label %IB_7 
IB_7:
  %_iv3 = add   i32 %_v2, 1 
  br label %IB_8 
IB_8:
  %_v3 = bitcast i32 %_iv3 to i32 
  br label %IB_9 
IB_9:
  %_iv4 = add   i32 %_v3, 1 
  br label %IB_10 
IB_10:
  %_v4 = bitcast i32 %_iv4 to i32 
  br label %IB_11 
IB_11:
  %_iv5 = add   i32 %_v4, 1 
  br label %IB_12 
IB_12:
  %_v5 = bitcast i32 %_iv5 to i32 
  br label %IB_13 
IB_13:
  %_iv6 = add   i32 %_v5, 1 
  br label %IB_14 
IB_14:
  %_v6 = bitcast i32 %_iv6 to i32 
  br label %IB_15 
IB_15:
  %_iv7 = add   i32 %_v6, 1 
  br label %IB_16 
IB_16:
  %_v7 = bitcast i32 %_iv7 to i32 
  br label %IB_17 
IB_17:
  %_iv8 = add   i32 %_v7, 1 
  br label %IB_18 
IB_18:
  %_v8 = bitcast i32 %_iv8 to i32 
  br label %IB_19 
IB_19:
  %_iv9 = add   i32 %_v8, 1 
  br label %IB_20 
IB_20:
  %_v9 = bitcast i32 %_iv9 to i32 
  br label %IB_21 
IB_21:
  %_iv10 = add   i32 %_v9, 1 
  br label %IB_22 
IB_22:
  %_v10 = bitcast i32 %_iv10 to i32 
  br label %IB_23 
IB_23:
  %_iv11 = add   i32 %_v10, 1 
  br label %IB_24 
IB_24:
  %_v11 = bitcast i32 %_iv11 to i32 
  br label %IB_25 
IB_25:
  %_iv12 = add   i32 %_v11, 1 
  br label %IB_26 
IB_26:
  %_v12 = bitcast i32 %_iv12 to i32 
  br label %IB_27 
IB_27:
  %_iv13 = add   i32 %_v12, 1 
  br label %IB_28 
IB_28:
  %_v13 = bitcast i32 %_iv13 to i32 
  br label %IB_29 
IB_29:
  %_iv14 = add   i32 %_v13, 1 
  br label %IB_30 
IB_30:
  %_v14 = bitcast i32 %_iv14 to i32 
  br label %IB_31 
IB_31:
  %_iv15 = add   i32 %_v14, 1 
  br label %IB_32 
IB_32:
  %_v15 = bitcast i32 %_iv15 to i32 
  br label %IB_33 
IB_33:
  %_iv16 = add   i32 %_v15, 1 
  br label %IB_34 
IB_34:
  %_v16 = bitcast i32 %_iv16 to i32 
  br label %IB_35 
IB_35:
  %_iv17 = add   i32 %_v16, 1 
  br label %IB_36 
IB_36:
  %_v17 = bitcast i32 %_iv17 to i32 
  br label %IB_37 
IB_37:
  %_iv18 = add   i32 %_v17, 1 
  br label %IB_38 
IB_38:
  %_v18 = bitcast i32 %_iv18 to i32 
  br label %IB_39 
IB_39:
  %_iv19 = add   i32 %_v18, 1 
  br label %IB_40 
IB_40:
  %_v19 = bitcast i32 %_iv19 to i32 
  br label %IB_41 
IB_41:
  %_iv20 = add   i32 %_v19, 1 
  br label %IB_42 
IB_42:
  %_v20 = bitcast i32 %_iv20 to i32 
  br label %IB_43 
IB_43:
  %_iv21 = add   i32 %_v20, 1 
  br label %IB_44 
IB_44:
  %_v21 = bitcast i32 %_iv21 to i32 
  br label %IB_45 
IB_45:
  %_iv22 = add   i32 %_v21, 1 
  br label %IB_46 
IB_46:
  %_v22 = bitcast i32 %_iv22 to i32 
  br label %IB_47 
IB_47:
  %_iv23 = add   i32 %_v22, 1 
  br label %IB_48 
IB_48:
  %_v23 = bitcast i32 %_iv23 to i32 
  br label %IB_49 
IB_49:
  %_iv24 = add   i32 %_v23, 1 
  br label %IB_50 
IB_50:
  %_v24 = bitcast i32 %_iv24 to i32 
  br label %IB_51 
IB_51:
  %_iv25 = add   i32 %_v24, 1 
  br label %IB_52 
IB_52:
  %_v25 = bitcast i32 %_iv25 to i32 
  br label %IB_53 
IB_53:
  %_iv26 = add   i32 %_v25, 1 
  br label %IB_54 
IB_54:
  %_v26 = bitcast i32 %_iv26 to i32 
  br label %IB_55 
IB_55:
  %_iv27 = add   i32 %_v26, 1 
  br label %IB_56 
IB_56:
  %_v27 = bitcast i32 %_iv27 to i32 
  br label %IB_57 
IB_57:
  %_iv28 = add   i32 %_v27, 1 
  br label %IB_58 
IB_58:
  %_v28 = bitcast i32 %_iv28 to i32 
  br label %IB_59 
IB_59:
  %_iv29 = add   i32 %_v28, 1 
  br label %IB_60 
IB_60:
  %_v29 = bitcast i32 %_iv29 to i32 
  br label %IB_61 
IB_61:
  %_iv30 = add   i32 %_v29, 1 
  br label %IB_62 
IB_62:
  %_v30 = bitcast i32 %_iv30 to i32 
  br label %IB_63 
IB_63:
  %_iv31 = add   i32 %_v30, 1 
  br label %IB_64 
IB_64:
  %_v31 = bitcast i32 %_iv31 to i32 
  br label %IB_65 
IB_65:
  %_iv32 = add   i32 %_v31, 1 
  br label %IB_66 
IB_66:
  %_v32 = bitcast i32 %_iv32 to i32 
  br label %IB_67 
IB_67:
  %_iv33 = add   i32 %_v32, 1 
  br label %IB_68 
IB_68:
  %_v33 = bitcast i32 %_iv33 to i32 
  br label %IB_69 
IB_69:
  %_iv34 = add   i32 %_v33, 1 
  br label %IB_70 
IB_70:
  %_v34 = bitcast i32 %_iv34 to i32 
  br label %IB_71 
IB_71:
  %_iv35 = add   i32 %_v34, 1 
  br label %IB_72 
IB_72:
  %_v35 = bitcast i32 %_iv35 to i32 
  br label %IB_73 
IB_73:
  %_iv36 = add   i32 %_v35, 1 
  br label %IB_74 
IB_74:
  %_v36 = bitcast i32 %_iv36 to i32 
  br label %IB_75 
IB_75:
  %_iv37 = add   i32 %_v36, 1 
  br label %IB_76 
IB_76:
  %_v37 = bitcast i32 %_iv37 to i32 
  br label %IB_77 
IB_77:
  %_iv38 = add   i32 %_v37, 1 
  br label %IB_78 
IB_78:
  %_v38 = bitcast i32 %_iv38 to i32 
  br label %IB_79 
IB_79:
  %_iv39 = add   i32 %_v38, 1 
  br label %IB_80 
IB_80:
  %_v39 = bitcast i32 %_iv39 to i32 
  br label %IB_81 
IB_81:
  %_iv40 = add   i32 %_v39, 1 
  br label %IB_82 
IB_82:
  %_v40 = bitcast i32 %_iv40 to i32 
  br label %IB_83 
IB_83:
  %_iv41 = add   i32 %_v40, 1 
  br label %IB_84 
IB_84:
  %_v41 = bitcast i32 %_iv41 to i32 
  br label %IB_85 
IB_85:
  %_iv42 = add   i32 %_v41, 1 
  br label %IB_86 
IB_86:
  %_v42 = bitcast i32 %_iv42 to i32 
  br label %IB_87 
IB_87:
  %_iv43 = add   i32 %_v42, 1 
  br label %IB_88 
IB_88:
  %_v43 = bitcast i32 %_iv43 to i32 
  br label %IB_89 
IB_89:
  %_iv44 = add   i32 %_v43, 1 
  br label %IB_90 
IB_90:
  %_v44 = bitcast i32 %_iv44 to i32 
  br label %IB_91 
IB_91:
  %_iv45 = add   i32 %_v44, 1 
  br label %IB_92 
IB_92:
  %_v45 = bitcast i32 %_iv45 to i32 
  br label %IB_93 
IB_93:
  %_iv46 = add   i32 %_v45, 1 
  br label %IB_94 
IB_94:
  %_v46 = bitcast i32 %_iv46 to i32 
  br label %IB_95 
IB_95:
  %_iv47 = add   i32 %_v46, 1 
  br label %IB_96 
IB_96:
  %_v47 = bitcast i32 %_iv47 to i32 
  br label %IB_97 
IB_97:
  %_iv48 = add   i32 %_v47, 1 
  br label %IB_98 
IB_98:
  %_v48 = bitcast i32 %_iv48 to i32 
  br label %IB_99 
IB_99:
  %_iv49 = add   i32 %_v48, 1 
  br label %IB_100 
IB_100:
  %_v49 = bitcast i32 %_iv49 to i32 
  br label %IB_101 
IB_101:
  %_iv50 = add   i32 %_v49, 1 
  br label %IB_102 
IB_102:
  %_v50 = bitcast i32 %_iv50 to i32 
  br label %IB_103 
IB_103:
  %_iv51 = add   i32 %_v50, 1 
  br label %IB_104 
IB_104:
  %_v51 = bitcast i32 %_iv51 to i32 
  br label %IB_105 
IB_105:
  %_iv52 = add   i32 %_v51, 1 
  br label %IB_106 
IB_106:
  %_v52 = bitcast i32 %_iv52 to i32 
  br label %IB_107 
IB_107:
  %_iv53 = add   i32 %_v52, 1 
  br label %IB_108 
IB_108:
  %_v53 = bitcast i32 %_iv53 to i32 
  br label %IB_109 
IB_109:
  %_iv54 = add   i32 %_v53, 1 
  br label %IB_110 
IB_110:
  %_v54 = bitcast i32 %_iv54 to i32 
  br label %IB_111 
IB_111:
  %_iv55 = add   i32 %_v54, 1 
  br label %IB_112 
IB_112:
  %_v55 = bitcast i32 %_iv55 to i32 
  br label %IB_113 
IB_113:
  %_iv56 = add   i32 %_v55, 1 
  br label %IB_114 
IB_114:
  %_v56 = bitcast i32 %_iv56 to i32 
  br label %IB_115 
IB_115:
  %_iv57 = add   i32 %_v56, 1 
  br label %IB_116 
IB_116:
  %_v57 = bitcast i32 %_iv57 to i32 
  br label %IB_117 
IB_117:
  %_iv58 = add   i32 %_v57, 1 
  br label %IB_118 
IB_118:
  %_v58 = bitcast i32 %_iv58 to i32 
  br label %IB_119 
IB_119:
  %_iv59 = add   i32 %_v58, 1 
  br label %IB_120 
IB_120:
  %_v59 = bitcast i32 %_iv59 to i32 
  br label %IB_121 
IB_121:
  %_iv60 = add   i32 %_v59, 1 
  br label %IB_122 
IB_122:
  %_v60 = bitcast i32 %_iv60 to i32 
  br label %IB_123 
IB_123:
  %_iv61 = add   i32 %_v60, 1 
  br label %IB_124 
IB_124:
  %_v61 = bitcast i32 %_iv61 to i32 
  br label %IB_125 
IB_125:
  %_iv62 = add   i32 %_v61, 1 
  br label %IB_126 
IB_126:
  %_v62 = bitcast i32 %_iv62 to i32 
  br label %IB_127 
IB_127:
  %_iv63 = add   i32 %_v62, 1 
  br label %IB_128 
IB_128:
  %_v63 = bitcast i32 %_iv63 to i32 
  br label %IB_129 
IB_129:
  %_iv64 = add   i32 %_v63, 1 
  br label %IB_130 
IB_130:
  %_v64 = bitcast i32 %_iv64 to i32 
  br label %IB_131 
IB_131:
  %_iv65 = add   i32 %_v64, 1 
  br label %IB_132 
IB_132:
  %_v65 = bitcast i32 %_iv65 to i32 
  br label %IB_133 
IB_133:
  %_iv66 = add   i32 %_v65, 1 
  br label %IB_134 
IB_134:
  %_v66 = bitcast i32 %_iv66 to i32 
  br label %IB_135 
IB_135:
  %_iv67 = add   i32 %_v66, 1 
  br label %IB_136 
IB_136:
  %_v67 = bitcast i32 %_iv67 to i32 
  br label %IB_137 
IB_137:
  %_iv68 = add   i32 %_v67, 1 
  br label %IB_138 
IB_138:
  %_v68 = bitcast i32 %_iv68 to i32 
  br label %IB_139 
IB_139:
  %_iv69 = add   i32 %_v68, 1 
  br label %IB_140 
IB_140:
  %_v69 = bitcast i32 %_iv69 to i32 
  br label %IB_141 
IB_141:
  %_iv70 = add   i32 %_v69, 1 
  br label %IB_142 
IB_142:
  %_v70 = bitcast i32 %_iv70 to i32 
  br label %IB_143 
IB_143:
  %_iv71 = add   i32 %_v70, 1 
  br label %IB_144 
IB_144:
  %_v71 = bitcast i32 %_iv71 to i32 
  br label %IB_145 
IB_145:
  %_iv72 = add   i32 %_v71, 1 
  br label %IB_146 
IB_146:
  %_v72 = bitcast i32 %_iv72 to i32 
  br label %IB_147 
IB_147:
  %_iv73 = add   i32 %_v72, 1 
  br label %IB_148 
IB_148:
  %_v73 = bitcast i32 %_iv73 to i32 
  br label %IB_149 
IB_149:
  %_iv74 = add   i32 %_v73, 1 
  br label %IB_150 
IB_150:
  %_v74 = bitcast i32 %_iv74 to i32 
  br label %IB_151 
IB_151:
  %_iv75 = add   i32 %_v74, 1 
  br label %IB_152 
IB_152:
  %_v75 = bitcast i32 %_iv75 to i32 
  br label %IB_153 
IB_153:
  %_iv76 = add   i32 %_v75, 1 
  br label %IB_154 
IB_154:
  %_v76 = bitcast i32 %_iv76 to i32 
  br label %IB_155 
IB_155:
  %_iv77 = add   i32 %_v76, 1 
  br label %IB_156 
IB_156:
  %_v77 = bitcast i32 %_iv77 to i32 
  br label %IB_157 
IB_157:
  %_iv78 = add   i32 %_v77, 1 
  br label %IB_158 
IB_158:
  %_v78 = bitcast i32 %_iv78 to i32 
  br label %IB_159 
IB_159:
  %_iv79 = add   i32 %_v78, 1 
  br label %IB_160 
IB_160:
  %_v79 = bitcast i32 %_iv79 to i32 
  br label %IB_161 
IB_161:
  %_iv80 = add   i32 %_v79, 1 
  br label %IB_162 
IB_162:
  %_v80 = bitcast i32 %_iv80 to i32 
  br label %IB_163 
IB_163:
  %_iv81 = add   i32 %_v80, 1 
  br label %IB_164 
IB_164:
  %_v81 = bitcast i32 %_iv81 to i32 
  br label %IB_165 
IB_165:
  %_iv82 = add   i32 %_v81, 1 
  br label %IB_166 
IB_166:
  %_v82 = bitcast i32 %_iv82 to i32 
  br label %IB_167 
IB_167:
  %_iv83 = add   i32 %_v82, 1 
  br label %IB_168 
IB_168:
  %_v83 = bitcast i32 %_iv83 to i32 
  br label %IB_169 
IB_169:
  %_iv84 = add   i32 %_v83, 1 
  br label %IB_170 
IB_170:
  %_v84 = bitcast i32 %_iv84 to i32 
  br label %IB_171 
IB_171:
  %_iv85 = add   i32 %_v84, 1 
  br label %IB_172 
IB_172:
  %_v85 = bitcast i32 %_iv85 to i32 
  br label %IB_173 
IB_173:
  %_iv86 = add   i32 %_v85, 1 
  br label %IB_174 
IB_174:
  %_v86 = bitcast i32 %_iv86 to i32 
  br label %IB_175 
IB_175:
  %_iv87 = add   i32 %_v86, 1 
  br label %IB_176 
IB_176:
  %_v87 = bitcast i32 %_iv87 to i32 
  br label %IB_177 
IB_177:
  %_iv88 = add   i32 %_v87, 1 
  br label %IB_178 
IB_178:
  %_v88 = bitcast i32 %_iv88 to i32 
  br label %IB_179 
IB_179:
  %_iv89 = add   i32 %_v88, 1 
  br label %IB_180 
IB_180:
  %_v89 = bitcast i32 %_iv89 to i32 
  br label %IB_181 
IB_181:
  %_iv90 = add   i32 %_v89, 1 
  br label %IB_182 
IB_182:
  %_v90 = bitcast i32 %_iv90 to i32 
  br label %IB_183 
IB_183:
  %_iv91 = add   i32 %_v90, 1 
  br label %IB_184 
IB_184:
  %_v91 = bitcast i32 %_iv91 to i32 
  br label %IB_185 
IB_185:
  %_iv92 = add   i32 %_v91, 1 
  br label %IB_186 
IB_186:
  %_v92 = bitcast i32 %_iv92 to i32 
  br label %IB_187 
IB_187:
  %_iv93 = add   i32 %_v92, 1 
  br label %IB_188 
IB_188:
  %_v93 = bitcast i32 %_iv93 to i32 
  br label %IB_189 
IB_189:
  %_iv94 = add   i32 %_v93, 1 
  br label %IB_190 
IB_190:
  %_v94 = bitcast i32 %_iv94 to i32 
  br label %IB_191 
IB_191:
  %_iv95 = add   i32 %_v94, 1 
  br label %IB_192 
IB_192:
  %_v95 = bitcast i32 %_iv95 to i32 
  br label %IB_193 
IB_193:
  %_iv96 = add   i32 %_v95, 1 
  br label %IB_194 
IB_194:
  %_v96 = bitcast i32 %_iv96 to i32 
  br label %IB_195 
IB_195:
  %_iv97 = add   i32 %_v96, 1 
  br label %IB_196 
IB_196:
  %_v97 = bitcast i32 %_iv97 to i32 
  br label %IB_197 
IB_197:
  %_iv98 = add   i32 %_v97, 1 
  br label %IB_198 
IB_198:
  %_v98 = bitcast i32 %_iv98 to i32 
  br label %IB_199 
IB_199:
  %_iv99 = add   i32 %_v98, 1 
  br label %IB_200 
IB_200:
  %_v99 = bitcast i32 %_iv99 to i32 
  br label %IB_201 
IB_201:
  %_iv100 = add   i32 %_v99, 1 
  br label %IB_202 
IB_202:
  %_v100 = bitcast i32 %_iv100 to i32 
  br label %IB_203 
IB_203:
  %_iv101 = add   i32 %_v100, 1 
  br label %IB_204 
IB_204:
  %_v101 = bitcast i32 %_iv101 to i32 
  br label %IB_205 
IB_205:
  %_iv102 = add   i32 %_v101, 1 
  br label %IB_206 
IB_206:
  %_v102 = bitcast i32 %_iv102 to i32 
  br label %IB_207 
IB_207:
  %_iv103 = add   i32 %_v102, 1 
  br label %IB_208 
IB_208:
  %_v103 = bitcast i32 %_iv103 to i32 
  br label %IB_209 
IB_209:
  %_iv104 = add   i32 %_v103, 1 
  br label %IB_210 
IB_210:
  %_v104 = bitcast i32 %_iv104 to i32 
  br label %IB_211 
IB_211:
  %_iv105 = add   i32 %_v104, 1 
  br label %IB_212 
IB_212:
  %_v105 = bitcast i32 %_iv105 to i32 
   call ccc  void  @printInt(i32  %_v105)  
  ret i32 0 
}