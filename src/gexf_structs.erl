-module(gexf_structs).

-include_lib("gexf/src/gexf.hrl").


basic() ->
#'_document-gexf'{version = "1.2",
    graph = 
        #'graph-content'{
            defaultedgetype = "directed",
            mode = "static",
            choice = 
                [#'graph-content/CH1'{
                     choice = 
                         #'nodes-content'{
                             node = 
                                 [#'node-content'{id = "0",label = "Hello"},
                                  #'node-content'{id = "1",label = "Word"}]}},
                 #'graph-content/CH1'{
                     anyAttribs = [],
                     choice = 
                         #'edges-content'{
                             edge = 
                                 [#'edge-content'{id = "0",source = "0",target = "1"}]}}]}}.



dynamics() ->
#'_document-gexf'{
    anyAttribs =
        [{{"schemaLocation",
           "http://www.w3.org/2001/XMLSchema-instance"},
          "http://www.gexf.net/1.2draft http://www.gexf.net/1.2draft/gexf.xsd"}],
    version = "1.2",variant = undefined,
    meta =
        #'meta-content'{
            anyAttribs = [],lastmodifieddate = "2009-03-20",
            choice =
                [#'meta-content-creator'{
                     anyAttribs = [],creator = "Gexf.net"},
                 #'meta-content-description'{
                     anyAttribs = [],
                     description = "A Web network changing over time"}]},
    graph =
        #'graph-content'{
            anyAttribs = [],timeformat = "date",start = undefined,
            startopen = undefined,'end' = undefined,endopen = undefined,
            defaultedgetype = "directed",idtype = undefined,
            mode = "dynamic",
            choice =
                [#'attributes-content'{
                     anyAttribs = [],class = "node",mode = "static",
                     start = undefined,startopen = undefined,'end' = undefined,
                     endopen = undefined,
                     attribute =
                         [#'attribute-content'{
                              anyAttribs = [],id = "0",title = "url",type = "string",
                              choice = undefined},
                          #'attribute-content'{
                              anyAttribs = [],id = "1",title = "frog",type = "boolean",
                              choice =
                                  [#'attribute-content-default'{
                                       anyAttribs = [],default = "true"}]}]},
                 #'attributes-content'{
                     anyAttribs = [],class = "node",mode = "dynamic",
                     start = undefined,startopen = undefined,'end' = undefined,
                     endopen = undefined,
                     attribute =
                         [#'attribute-content'{
                              anyAttribs = [],id = "2",title = "indegree",type = "float",
                              choice = undefined}]},
                 #'graph-content/CH1'{
                     anyAttribs = [],
                     choice =
                         #'nodes-content'{
                             anyAttribs = [],count = undefined,
                             node =
                                 [#'node-content'{
                                      anyAttribs = [],start = "2009-03-01",startopen = undefined,
                                      'end' = undefined,endopen = undefined,pid = undefined,
                                      id = "0",label = "Gephi",
                                      choice =
                                          [#'attvalues-content'{
                                               anyAttribs = [],
                                               attvalue =
                                                   [#attvalue{
                                                        anyAttribs = [],for = "0",value = "http://gephi.org",
                                                        start = undefined,startopen = undefined,'end' = undefined,
                                                        endopen = undefined},
                                                    #attvalue{
                                                        anyAttribs = [],for = "2",value = "1",start = undefined,
                                                        startopen = undefined,'end' = undefined,
                                                        endopen = undefined}]}]},
                                  #'node-content'{
                                      anyAttribs = [],start = undefined,startopen = undefined,
                                      'end' = undefined,endopen = undefined,pid = undefined,
                                      id = "1",label = "Network",
                                      choice =
                                          [#'attvalues-content'{
                                               anyAttribs = [],
                                               attvalue =
                                                   [#attvalue{
                                                        anyAttribs = [],for = "2",value = "1",start = undefined,
                                                        startopen = undefined,'end' = "2009-03-01",
                                                        endopen = undefined},
                                                    #attvalue{
                                                        anyAttribs = [],for = "2",value = "2",start = "2009-03-01",
                                                        startopen = undefined,'end' = "2009-03-10",
                                                        endopen = undefined},
                                                    #attvalue{
                                                        anyAttribs = [],for = "2",value = "1",start = "2009-03-10",
                                                        startopen = undefined,'end' = undefined,
                                                        endopen = undefined}]}]},
                                  #'node-content'{
                                      anyAttribs = [],start = undefined,startopen = undefined,
                                      'end' = undefined,endopen = undefined,pid = undefined,
                                      id = "2",label = "Visualization",
                                      choice =
                                          [#'attvalues-content'{
                                               anyAttribs = [],
                                               attvalue =
                                                   [#attvalue{
                                                        anyAttribs = [],for = "2",value = "0",start = undefined,
                                                        startopen = undefined,'end' = "2009-03-01",
                                                        endopen = undefined},
                                                    #attvalue{
                                                        anyAttribs = [],for = "2",value = "1",start = "2009-03-01",
                                                        startopen = undefined,'end' = undefined,
                                                        endopen = undefined}]},
                                           #'spells-content'{
                                               anyAttribs = [],
                                               spell =
                                                   [#spell{
                                                        anyAttribs = [],start = undefined,startopen = undefined,
                                                        'end' = "2009-03-01",endopen = undefined},
                                                    #spell{
                                                        anyAttribs = [],start = "2009-03-05",startopen = undefined,
                                                        'end' = "2009-03-10",endopen = undefined}]}]},
                                  #'node-content'{
                                      anyAttribs = [],start = undefined,startopen = undefined,
                                      'end' = undefined,endopen = undefined,pid = undefined,
                                      id = "3",label = "Graph",
                                      choice =
                                          [#'attvalues-content'{
                                               anyAttribs = [],
                                               attvalue =
                                                   [#attvalue{
                                                        anyAttribs = [],for = "1",value = "false",start = undefined,
                                                        startopen = undefined,'end' = undefined,endopen = undefined},
                                                    #attvalue{
                                                        anyAttribs = [],for = "2",value = "0",start = undefined,
                                                        startopen = undefined,'end' = "2009-03-01",
                                                        endopen = undefined},
                                                    #attvalue{
                                                        anyAttribs = [],for = "2",value = "1",start = "2009-03-01",
                                                        startopen = undefined,'end' = undefined,
                                                        endopen = undefined}]}]}]}},
                 #'graph-content/CH1'{
                     anyAttribs = [],
                     choice =
                         #'edges-content'{
                             anyAttribs = [],count = undefined,
                             edge =
                                 [#'edge-content'{
                                      anyAttribs = [],start = "2009-03-01",startopen = undefined,
                                      'end' = undefined,endopen = undefined,id = "0",
                                      type = undefined,label = undefined,source = "0",
                                      target = "1",weight = undefined,choice = undefined},
                                  #'edge-content'{
                                      anyAttribs = [],start = "2009-03-01",startopen = undefined,
                                      'end' = "2009-03-10",endopen = undefined,id = "1",
                                      type = undefined,label = undefined,source = "0",
                                      target = "2",weight = undefined,choice = undefined},
                                  #'edge-content'{
                                      anyAttribs = [],start = "2009-03-01",startopen = undefined,
                                      'end' = undefined,endopen = undefined,id = "2",
                                      type = undefined,label = undefined,source = "1",
                                      target = "0",weight = undefined,choice = undefined},
                                  #'edge-content'{
                                      anyAttribs = [],start = undefined,startopen = undefined,
                                      'end' = "2009-03-10",endopen = undefined,id = "3",
                                      type = undefined,label = undefined,source = "2",
                                      target = "1",weight = undefined,choice = undefined},
                                  #'edge-content'{
                                      anyAttribs = [],start = "2009-03-01",startopen = undefined,
                                      'end' = undefined,endopen = undefined,id = "4",
                                      type = undefined,label = undefined,source = "0",
                                      target = "3",weight = undefined,choice = undefined}]}}]}}.

tree() ->
#'_document-gexf'{
    anyAttribs =
        [{{"schemaLocation",
           "http://www.w3.org/2001/XMLSchema-instance"},
          "http://www.gexf.net/1.2draft http://www.gexf.net/1.2draft/gexf.xsd"}],
    version = "1.2",
    graph =
        #'graph-content'{
            defaultedgetype = "directed",
            choice =
                [#'attributes-content'{
                     class = "node",
                     attribute =
                         [#'attribute-content'{
                              id = "0",title = "url",type = "string"},
                          #'attribute-content'{
                              id = "1",title = "indegree",type = "float"},
                          #'attribute-content'{
                              id = "2",title = "frog",type = "boolean",
                              choice =
                                  [#'attribute-content-default'{
                                       default = "true"}]}]},
                 #'graph-content/CH1'{
                     choice =
                         #'nodes-content'{
                             node =
                                 [#'node-content'{
                                      id = "0",label = "Gephi",
                                      choice =
                                          [#'attvalues-content'{
                                               attvalue =
                                                   [#attvalue{
                                                        for = "0",value = "http://gephi.org"},
                                                    #attvalue{
                                                        for = "1",value = "1"}]}]},
                                  #'node-content'{
                                      id = "1",label = "Webatlas",
                                      choice =
                                          [#'attvalues-content'{
                                               attvalue =
                                                   [#attvalue{
                                                        for = "0",value = "http://webatlas.fr"
                                                        },
                                                    #attvalue{
                                                        for = "1",value = "2"
                                                        }]}]},
                                  #'node-content'{
                                      id = "2",label = "RTGI",
                                      choice =
                                          [#'attvalues-content'{
                                               attvalue =
                                                   [#attvalue{
                                                        for = "0",value = "http://rtgi.fr"
                                                        },
                                                    #attvalue{
                                                        for = "1",value = "1"
                                                        }]}]},
                                  #'node-content'{
                                      id = "3",label = "BarabasiLab",
                                      choice =
                                          [#'attvalues-content'{
                                               
                                               attvalue =
                                                   [#attvalue{
                                                        for = "0",value = "http://barabasilab.com"
                                                        },
                                                    #attvalue{
                                                        for = "1",value = "1"
                                                        },
                                                    #attvalue{
                                                        for = "2",value = "false"
                                                        }]}]}]}},
                 #'graph-content/CH1'{
                     choice =
                         #'edges-content'{
                             edge =
                                 [#'edge-content'{
                                      id = "0",
                                      source = "0",
                                      target = "1"},
                                  #'edge-content'{
                                      id = "1",
                                      source = "0",
                                      target = "2"},
                                  #'edge-content'{
                                      id = "2",
                                      source = "1",
                                      target = "0"},
                                  #'edge-content'{
                                      id = "3",
                                      source = "2",
                                      target = "1"},
                                  #'edge-content'{
                                      id = "4",
                                      source = "0",
                                      target = "3"}]}}]}}.





