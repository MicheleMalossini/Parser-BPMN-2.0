library(xml2)

parseBPMN <- function(xmlFilePath) {
  # Read the XML file
  xmlContent <- readLines(xmlFilePath, warn = FALSE, encoding = "UTF-8")
  
  # Convert XML content to a single string
  xmlString <- paste(xmlContent, collapse = "\n")
  
  # Parse the XML content
  doc <- xml2::read_xml(xmlString)
  
  # Rest of the parsing logic...
  # (process information, pool information, activity information, flow information)
  # ...
  # Extract process information
  processNode <- xml_find_first(doc, ".//bpmn:process")
  processId <- xml_attr(processNode, "id")
  processName <- xml_attr(processNode, "name")
  processDocumentation <- xml_text(xml_find_first(processNode, ".//bpmn:documentation"))
  
  # Extract pools information
  poolNodes <- xml_find_all(doc, ".//bpmn:collaboration/bpmn:participant")
  pools <- lapply(poolNodes, function(node) {
    poolId <- xml_attr(node, "id")
    poolName <- xml_attr(node, "name")
    
    list(id = poolId, name = poolName)
  })
  
  
  
  # Extract activity information
  activityNodes <- xml_find_all(doc, ".//bpmn:task | .//bpmn:subProcess")
  activities <- lapply(activityNodes, function(node) {
    activityId <- xml_attr(node, "id")
    activityName <- xml_attr(node, "name")
    # Extract additional activity properties as needed
    
    list(id = activityId, name = activityName)
  })
  
  # Extract flow information
  flowNodes <- xml_find_all(doc, ".//bpmn:sequenceFlow")
  flows <- lapply(flowNodes, function(node) {
    flowId <- xml_attr(node, "id")
    sourceRef <- xml_attr(node, "sourceRef")
    targetRef <- xml_attr(node, "targetRef")
    # Extract additional flow properties as needed
    
    list(id = flowId, source = sourceRef, target = targetRef)
  })
  
  
  # Return the parsed data
  list(
    process = list(id = processId, name = processName, documentation = processDocumentation),
    pools = pools,
    activities = activities,
    flows = flows
  )
}