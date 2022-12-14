package clangast.stubs

import clangast.*
import clangast.given
import clangast.decl.{CFunctionDecl, CInclude}
import clangast.expr.binaryop.{CAssignmentExpr, CNotEqualsExpr}
import clangast.expr.{CConditionalOperator, CExpr, CMemberExpr, CNullLiteral, CParenExpr}
import clangast.stmt.{CCompoundStmt, CForStmt}
import clangast.types.{CType, CTypedefType}
import compiler.context.TranslationContext

object CJSONH extends CLibraryStub {
  override val include: CInclude = CInclude("cJSON.h", true)

  def cJSON(using TranslationContext): CType = includeStub(CTypedefType("cJSON"))
  def valuestring(expr: CExpr)(using TranslationContext): CExpr =
    includeStub(CMemberExpr(CParenExpr(expr), "valuestring", true))
  def valueint(expr: CExpr)(using TranslationContext): CExpr =
    includeStub(CMemberExpr(CParenExpr(expr), "valueint", true))
  def valuedouble(expr: CExpr)(using TranslationContext): CExpr =
    includeStub(CMemberExpr(CParenExpr(expr), "valuedouble", true))

  def cJSON_Version(using TranslationContext): CFunctionDecl   = includeStub(CFunctionStub("cJSON_Version"))
  def cJSON_InitHooks(using TranslationContext): CFunctionDecl = includeStub(CFunctionStub("cJSON_InitHooks"))
  def cJSON_Parse(using TranslationContext): CFunctionDecl     = includeStub(CFunctionStub("cJSON_Parse"))
  def cJSON_ParseWithLength(using TranslationContext): CFunctionDecl =
    includeStub(CFunctionStub("cJSON_ParseWithLength"))
  def cJSON_ParseWithOpts(using TranslationContext): CFunctionDecl = includeStub(CFunctionStub("cJSON_ParseWithOpts"))
  def cJSON_ParseWithLengthOpts(using TranslationContext): CFunctionDecl =
    includeStub(CFunctionStub("cJSON_ParseWithLengthOpts"))
  def cJSON_Print(using TranslationContext): CFunctionDecl = includeStub(CFunctionStub("cJSON_Print"))
  def cJSON_PrintUnformatted(using TranslationContext): CFunctionDecl =
    includeStub(CFunctionStub("cJSON_PrintUnformatted"))
  def cJSON_PrintBuffered(using TranslationContext): CFunctionDecl = includeStub(CFunctionStub("cJSON_PrintBuffered"))
  def cJSON_PrintPreallocated(using TranslationContext): CFunctionDecl =
    includeStub(CFunctionStub("cJSON_PrintPreallocated"))
  def cJSON_Delete(using TranslationContext): CFunctionDecl        = includeStub(CFunctionStub("cJSON_Delete"))
  def cJSON_GetArraySize(using TranslationContext): CFunctionDecl  = includeStub(CFunctionStub("cJSON_GetArraySize"))
  def cJSON_GetArrayItem(using TranslationContext): CFunctionDecl  = includeStub(CFunctionStub("cJSON_GetArrayItem"))
  def cJSON_GetObjectItem(using TranslationContext): CFunctionDecl = includeStub(CFunctionStub("cJSON_GetObjectItem"))
  def cJSON_GetObjectItemCaseSensitive(using TranslationContext): CFunctionDecl =
    includeStub(CFunctionStub("cJSON_GetObjectItemCaseSensitive"))
  def cJSON_HasObjectItem(using TranslationContext): CFunctionDecl  = includeStub(CFunctionStub("cJSON_HasObjectItem"))
  def cJSON_GetErrorPtr(using TranslationContext): CFunctionDecl    = includeStub(CFunctionStub("cJSON_GetErrorPtr"))
  def cJSON_GetStringValue(using TranslationContext): CFunctionDecl = includeStub(CFunctionStub("cJSON_GetStringValue"))
  def cJSON_GetNumberValue(using TranslationContext): CFunctionDecl = includeStub(CFunctionStub("cJSON_GetNumberValue"))
  def cJSON_IsInvalid(using TranslationContext): CFunctionDecl      = includeStub(CFunctionStub("cJSON_IsInvalid"))
  def cJSON_IsFalse(using TranslationContext): CFunctionDecl        = includeStub(CFunctionStub("cJSON_IsFalse"))
  def cJSON_IsTrue(using TranslationContext): CFunctionDecl         = includeStub(CFunctionStub("cJSON_IsTrue"))
  def cJSON_IsBool(using TranslationContext): CFunctionDecl         = includeStub(CFunctionStub("cJSON_IsBool"))
  def cJSON_IsNull(using TranslationContext): CFunctionDecl         = includeStub(CFunctionStub("cJSON_IsNull"))
  def cJSON_IsNumber(using TranslationContext): CFunctionDecl       = includeStub(CFunctionStub("cJSON_IsNumber"))
  def cJSON_IsString(using TranslationContext): CFunctionDecl       = includeStub(CFunctionStub("cJSON_IsString"))
  def cJSON_IsArray(using TranslationContext): CFunctionDecl        = includeStub(CFunctionStub("cJSON_IsArray"))
  def cJSON_IsObject(using TranslationContext): CFunctionDecl       = includeStub(CFunctionStub("cJSON_IsObject"))
  def cJSON_IsRaw(using TranslationContext): CFunctionDecl          = includeStub(CFunctionStub("cJSON_IsRaw"))
  def cJSON_CreateNull(using TranslationContext): CFunctionDecl     = includeStub(CFunctionStub("cJSON_CreateNull"))
  def cJSON_CreateTrue(using TranslationContext): CFunctionDecl     = includeStub(CFunctionStub("cJSON_CreateTrue"))
  def cJSON_CreateFalse(using TranslationContext): CFunctionDecl    = includeStub(CFunctionStub("cJSON_CreateFalse"))
  def cJSON_CreateBool(using TranslationContext): CFunctionDecl     = includeStub(CFunctionStub("cJSON_CreateBool"))
  def cJSON_CreateNumber(using TranslationContext): CFunctionDecl   = includeStub(CFunctionStub("cJSON_CreateNumber"))
  def cJSON_CreateString(using TranslationContext): CFunctionDecl   = includeStub(CFunctionStub("cJSON_CreateString"))
  def cJSON_CreateRaw(using TranslationContext): CFunctionDecl      = includeStub(CFunctionStub("cJSON_CreateRaw"))
  def cJSON_CreateArray(using TranslationContext): CFunctionDecl    = includeStub(CFunctionStub("cJSON_CreateArray"))
  def cJSON_CreateObject(using TranslationContext): CFunctionDecl   = includeStub(CFunctionStub("cJSON_CreateObject"))
  def cJSON_CreateStringReference(using TranslationContext): CFunctionDecl =
    includeStub(CFunctionStub("cJSON_CreateStringReference"))
  def cJSON_CreateObjectReference(using TranslationContext): CFunctionDecl =
    includeStub(CFunctionStub("cJSON_CreateObjectReference"))
  def cJSON_CreateArrayReference(using TranslationContext): CFunctionDecl =
    includeStub(CFunctionStub("cJSON_CreateArrayReference"))
  def cJSON_CreateIntArray(using TranslationContext): CFunctionDecl = includeStub(CFunctionStub("cJSON_CreateIntArray"))
  def cJSON_CreateFloatArray(using TranslationContext): CFunctionDecl =
    includeStub(CFunctionStub("cJSON_CreateFloatArray"))
  def cJSON_CreateDoubleArray(using TranslationContext): CFunctionDecl =
    includeStub(CFunctionStub("cJSON_CreateDoubleArray"))
  def cJSON_CreateStringArray(using TranslationContext): CFunctionDecl =
    includeStub(CFunctionStub("cJSON_CreateStringArray"))
  def cJSON_AddItemToArray(using TranslationContext): CFunctionDecl = includeStub(CFunctionStub("cJSON_AddItemToArray"))
  def cJSON_AddItemToObject(using TranslationContext): CFunctionDecl =
    includeStub(CFunctionStub("cJSON_AddItemToObject"))
  def cJSON_AddItemToObjectCS(using TranslationContext): CFunctionDecl =
    includeStub(CFunctionStub("cJSON_AddItemToObjectCS"))
  def cJSON_AddItemReferenceToArray(using TranslationContext): CFunctionDecl =
    includeStub(CFunctionStub("cJSON_AddItemReferenceToArray"))
  def cJSON_AddItemReferenceToObject(using TranslationContext): CFunctionDecl =
    includeStub(CFunctionStub("cJSON_AddItemReferenceToObject"))
  def cJSON_DetachItemViaPointer(using TranslationContext): CFunctionDecl =
    includeStub(CFunctionStub("cJSON_DetachItemViaPointer"))
  def cJSON_DetachItemFromArray(using TranslationContext): CFunctionDecl =
    includeStub(CFunctionStub("cJSON_DetachItemFromArray"))
  def cJSON_DeleteItemFromArray(using TranslationContext): CFunctionDecl =
    includeStub(CFunctionStub("cJSON_DeleteItemFromArray"))
  def cJSON_DetachItemFromObject(using TranslationContext): CFunctionDecl =
    includeStub(CFunctionStub("cJSON_DetachItemFromObject"))
  def cJSON_DetachItemFromObjectCaseSensitive(using TranslationContext): CFunctionDecl =
    includeStub(CFunctionStub("cJSON_DetachItemFromObjectCaseSensitive"))
  def cJSON_DeleteItemFromObject(using TranslationContext): CFunctionDecl =
    includeStub(CFunctionStub("cJSON_DeleteItemFromObject"))
  def cJSON_DeleteItemFromObjectCaseSensitive(using TranslationContext): CFunctionDecl =
    includeStub(CFunctionStub("cJSON_DeleteItemFromObjectCaseSensitive"))
  def cJSON_InsertItemInArray(using TranslationContext): CFunctionDecl =
    includeStub(CFunctionStub("cJSON_InsertItemInArray"))
  def cJSON_ReplaceItemViaPointer(using TranslationContext): CFunctionDecl =
    includeStub(CFunctionStub("cJSON_ReplaceItemViaPointer"))
  def cJSON_ReplaceItemInArray(using TranslationContext): CFunctionDecl =
    includeStub(CFunctionStub("cJSON_ReplaceItemInArray"))
  def cJSON_ReplaceItemInObject(using TranslationContext): CFunctionDecl =
    includeStub(CFunctionStub("cJSON_ReplaceItemInObject"))
  def cJSON_ReplaceItemInObjectCaseSensitive(using TranslationContext): CFunctionDecl =
    includeStub(CFunctionStub("cJSON_ReplaceItemInObjectCaseSensitive"))
  def cJSON_Duplicate(using TranslationContext): CFunctionDecl = includeStub(CFunctionStub("cJSON_Duplicate"))
  def cJSON_Compare(using TranslationContext): CFunctionDecl   = includeStub(CFunctionStub("cJSON_Compare"))
  def cJSON_Minify(using TranslationContext): CFunctionDecl    = includeStub(CFunctionStub("cJSON_Minify"))
  def cJSON_AddNullToObject(using TranslationContext): CFunctionDecl =
    includeStub(CFunctionStub("cJSON_AddNullToObject"))
  def cJSON_AddTrueToObject(using TranslationContext): CFunctionDecl =
    includeStub(CFunctionStub("cJSON_AddTrueToObject"))
  def cJSON_AddFalseToObject(using TranslationContext): CFunctionDecl =
    includeStub(CFunctionStub("cJSON_AddFalseToObject"))
  def cJSON_AddBoolToObject(using TranslationContext): CFunctionDecl =
    includeStub(CFunctionStub("cJSON_AddBoolToObject"))
  def cJSON_AddNumberToObject(using TranslationContext): CFunctionDecl =
    includeStub(CFunctionStub("cJSON_AddNumberToObject"))
  def cJSON_AddStringToObject(using TranslationContext): CFunctionDecl =
    includeStub(CFunctionStub("cJSON_AddStringToObject"))
  def cJSON_AddRawToObject(using TranslationContext): CFunctionDecl = includeStub(CFunctionStub("cJSON_AddRawToObject"))
  def cJSON_AddObjectToObject(using TranslationContext): CFunctionDecl =
    includeStub(CFunctionStub("cJSON_AddObjectToObject"))
  def cJSON_AddArrayToObject(using TranslationContext): CFunctionDecl =
    includeStub(CFunctionStub("cJSON_AddArrayToObject"))
  def cJSON_SetNumberHelper(using TranslationContext): CFunctionDecl =
    includeStub(CFunctionStub("cJSON_SetNumberHelper"))
  def cJSON_SetValuestring(using TranslationContext): CFunctionDecl = includeStub(CFunctionStub("cJSON_SetValuestring"))
  def cJSON_malloc(using TranslationContext): CFunctionDecl         = includeStub(CFunctionStub("cJSON_malloc"))
  def cJSON_free(using TranslationContext): CFunctionDecl           = includeStub(CFunctionStub("cJSON_free"))

  def cJSON_ArrayForEach(element: CExpr, array: CExpr, body: CCompoundStmt)(using TranslationContext): CForStmt =
    CForStmt(
      Some(CAssignmentExpr(
        element,
        CConditionalOperator(
          CParenExpr(CNotEqualsExpr(array, CNullLiteral)),
          CMemberExpr(CParenExpr(array), "child", true),
          CNullLiteral
        )
      )),
      Some(CNotEqualsExpr(element, CNullLiteral)),
      Some(CAssignmentExpr(element, CMemberExpr(element, "next", true))),
      body
    )
}
