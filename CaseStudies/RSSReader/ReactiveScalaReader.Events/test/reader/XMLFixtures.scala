package reader

object XMLFixtures {
  val missingLinkChannel = <channel>
                             <title>feedTest feed</title>
                             <link></link>
                             <language>en-us</language>
                           </channel>

  val simpleChannel = <channel>
                        <title>feedTest feed</title>
                        <link>https://bitbucket.org/</link>
                        <language>en-us</language>
                      </channel>

  val simpleRSS = <rss>{simpleChannel}</rss>

  val simpleItem = <item>
                     <title>Test title</title>
                     <link>https://bitbucket.org/ipls/feedtest/commits/b09c86163df5</link>
                     <description>Test description</description>
                     <author>Markus Hauck</author>
                     <pubDate>Sat, 15 Dec 2012 13:26:19 +0000</pubDate>
                     <link>https://bitbucket.org/</link>
                   </item>

  val simpleItem2 = <item>
                     <title>Simple Item 2</title>
                     <link>https://bitbucket.org/ipls/feedtest/commits/b09c86163df5</link>
                     <description>Description for simple item 2</description>
                     <author>Markus Hauck</author>
                     <pubDate>Sun, 23 Dec 2012 23:12:00 +0000</pubDate>
                     <link>https://bitbucket.org/</link>
                   </item>

  val completeRSS = <rss>
                      <channel>
                        <title>feedTest feed</title>
                        <link>https://bitbucket.org/</link>
                        <language>en-us</language>
                        {simpleItem}
                      </channel>
                    </rss>

  val completeRSS2Items = <rss>
                            <channel>
                              <title>feedTest feed</title>
                              <link>https://bitbucket.org/</link>
                              <language>en-us</language>
                              {simpleItem}
                              {simpleItem2}
                            </channel>
                          </rss>

  val corruptDateItem = <item>
                          <title>Test title</title>
                          <link>https://bitbucket.org/ipls/feedtest/commits/b09c86163df5</link>
                          <description>Test description</description>
                          <author>Markus Hauck</author>
                          <pubDate>hier koennte ihre werbung stehen</pubDate>
                        </item>

  val missingLinkItem = <item>
                          <title>The link of this item is missing</title>
                          <description>Test description</description>
                          <author>Markus Hauck</author>
                        </item>

  val corruptItem= <item>nothing</item>

}
