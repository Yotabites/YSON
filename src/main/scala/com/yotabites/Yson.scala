package com.yotabites

import java.util
import org.json.{JSONArray, JSONObject, XML}
import scala.util.Try

/**
  * Created by sen on 11/21/2017.
  */

class Yson {
  private val map = new java.util.HashMap[String, String]
  private val MAP_INDEX_PREFIX = "["
  private val MAP_INDEX_SUFFIX = "]"
  private val TAG_DELIMITER = "."
  private val IS_NESTED = false

  def parseXml(xmlString: String, delimiter: String = TAG_DELIMITER, indexPrefix: String = MAP_INDEX_PREFIX,
               indexSuffix: String = MAP_INDEX_SUFFIX, isNested: Boolean = IS_NESTED): util.HashMap[String, String] =
    getMap(
      valString = xmlString,
      xmlFlag = true,
      delimiter = delimiter,
      indexPrefix = indexPrefix,
      indexSuffix = indexSuffix,
      isNested = isNested
    )

  def parseJson(jsonString: String, delimiter: String = TAG_DELIMITER, indexPrefix: String = MAP_INDEX_PREFIX,
                indexSuffix: String = MAP_INDEX_SUFFIX, isNested: Boolean = IS_NESTED): util.HashMap[String, String] =
    getMap(
      valString = jsonString,
      delimiter = delimiter,
      indexPrefix = indexPrefix,
      indexSuffix = indexSuffix,
      isNested = isNested
    )


  private def getMap(valString: String, xmlFlag: Boolean = false, delimiter: String, indexPrefix: String,
                     indexSuffix: String, isNested: Boolean): util.HashMap[String, String] =  Try {
    val jsonObj = if(xmlFlag) XML.toJSONObject(valString) else new JSONObject(valString)
    explodeJsonObject("", jsonObj, 0, delimiter, indexPrefix, indexSuffix, isNested)
  }.getOrElse(new util.HashMap[String, String]())


  private def explodeJsonArray(pKey: String, arr: JSONArray, delimiter: String, indexPrefix: String,
                               indexSuffix: String, isNested: Boolean): java.util.HashMap[String, String] = {
    def loopArray(jArr: JSONArray, jsonIndex: Int, mapIndex: Int, isIndexNeeded: Boolean): Unit =
      jArr.get(jsonIndex) match {
        case arr1: JSONArray => explodeJsonArray(pKey, arr1, delimiter, indexPrefix, indexSuffix, isNested)
        case obj: JSONObject => explodeJsonObject(pKey, obj, mapIndex, delimiter, indexPrefix, indexSuffix, isNested)
        case _ => val sufIndex = if (isIndexNeeded) indexPrefix + mapIndex + indexSuffix else ""
          val res = jArr.get(jsonIndex).toString
          map.put(pKey.toUpperCase.replace(":", delimiter) + sufIndex, res)
      }

    // If one element array, both indexes (JSON and MAP) are 0.
    // Otherwise, MAP_INDEX = JSON's + 1. Coz MAP index starts from '1'.
    def directLoopArray(index: Int, jSONArray: JSONArray): Unit = if (index < jSONArray.length) {
      loopArray(jSONArray, index, index + 1, isIndexNeeded = true)
      directLoopArray(index + 1, jSONArray)
    }

    // if(arr.length == 1) loopArray(arr, 0, 0) else for (i <- 0 until arr.length) { loopArray(arr, i, i + 1) }
    // until: Range => issues with Java 8 runtime error
    if(arr.length == 1)
      loopArray(arr, 0, 0, isIndexNeeded = false)
    else directLoopArray(0, arr)
    map
  }


  private def isJsonObj(string: String): Boolean = Try {new JSONObject(string)}.isSuccess


  private def explodeJsonObject(pKey: String, jsonObj: JSONObject, index: Int, delimiter: String, indexPrefix: String,
                                indexSuffix: String, isNested: Boolean): java.util.HashMap[String, String] = {
    val list = jsonObj.keys()
    while (list.hasNext) {
      val keyStr = list.next()
      val value = jsonObj.get(keyStr)

      val newIndex = if(index == 0) "" else indexPrefix + index + indexSuffix
      val newKey = if (pKey.nonEmpty) pKey + newIndex + delimiter + keyStr else keyStr + newIndex

      value match {
        case jsonObject: JSONObject =>
          explodeJsonObject(newKey, jsonObject, 0, delimiter, indexPrefix, indexSuffix, isNested)
        case arr: JSONArray => explodeJsonArray(newKey, arr, delimiter, indexPrefix, indexSuffix, isNested)
        case nestedJson: String if isNested && isJsonObj(nestedJson) =>
          explodeJsonObject(newKey, new JSONObject(nestedJson), 0, delimiter, indexPrefix, indexSuffix, isNested)
        case _ => map.put(newKey.toUpperCase.replace(":", delimiter), value.toString)
      }
    }
    map
  }
}
