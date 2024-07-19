# ## Server Introduction page 

# To do:
# add a contact email address
# add dashboard instructions
# add construct rationale to 'developing' page

###############################################.
## Button to return to previously visited tab ----
###############################################.

observeEvent(input$intro_back, {
  updateTabsetPanel(session, "intabset",
                    selected = rv$last_tab)
})


# observeEvents respond if the relevant actionButton is clicked on homepageTab
# The background text is rendered before any button is clicked.
observeEvent(input$background, {
  output$text <- renderText({
    paste("<h2>Background to the dashboard</h2>
            Mental health and wellbeing is a significant public health challenge for Scotland.
            <br><br>Mental health is a national priority because of its importance for growth, development, learning and resilience:
            it is associated with better physical health, positive interpersonal relationships, and more equitable and productive societies.
            <br><br>In their <a href=\"https://www.gov.scot/publications/mental-health-wellbeing-strategy/\" target=\"_blank\">
            <b>Mental Health and Wellbeing Strategy</b></a> the Scottish Government and COSLA set out their vision for a Scotland free from stigma and inequality,
            where everyone fulfils their right to achieve the best mental health and wellbeing possible.
            As part of the strategy the Scottish Government and COSLA want to see \"Better access to and use of evidence and data in policy and practice\".
            <br><br>As Scotland's public health body, we are working to support an intelligence-led approach to public mental health, 
            and to identifying effective action to maximise the mental wellbeing of the population of Scotland.
            Through the <a href=\"https://publichealthscotland.scot/our-areas-of-work/health-and-wellbeing/prevention-of-mental-ill-health-and-improved-wellbeing/mental-health-indicators/overview/\" target=\"_blank\">
            <b>Mental Health Indicators</b></a> project, we aim to make relevant data more accessible to local and national users.
            Better access to timely data will support efforts to improve population mental health, through more effective planning and policies, and more efficient resource allocation. 
            <br><br>Mental health indicator data for adults are presented in this dashboard. Adults are defined as individuals aged 16 years and over, unless otherwise stated. 
            Mental health indicator data for children and young people are presented in this dashboard (INSERT LINK WHEN PUBLISHED). 
            <br><br>
          ")
    
  })
  output$image <- renderUI({ "" })
},
ignoreNULL = FALSE) # so that this text loads initially, before a button is clicked

observeEvent(input$indicators, {
  output$text <- renderText({
    paste("<h2>The adult mental health indicator set</h2>
      The 55 indicators in the
            <a href=\"https://publichealthscotland.scot/our-areas-of-work/health-and-wellbeing/prevention-of-mental-ill-health-and-improved-wellbeing/mental-health-indicators/adult-mental-health-indicators/\" target=\"_blank\">
            <b>adult mental health indicator (MHI) set</b></a> are grouped into four high-level ‘domains’:
      <ul>
      <li>10 <b>Mental health outcomes:</b> mental wellbeing and mental health problems</li>
      <li>Contextual factors: a wide range of interconnected determinants (risk factors and protective factors) of mental health outcomes, that relate either to individuals, their communities, or the wider structural influences on them.
      <ul>
      <li>9 <b>Individual-level</b> factors</li>
      <li>11 <b>Community-level</b> factors</li>
      <li>25 <b>Structural</b> factors</li>
      </ul>
      </li>
      </ul>
      ")
  })
  
  output$image <- renderUI({ 
    tags$img(height = 600, width = 800, 
             src = "mhi_figure.png",
             alt="Image showing the 4 mental health indicator domains: 
                                  Mental health outcomes, Individual-level determinants, Community-level determinants, and Structural-level determinants.
                                  Beneath each domain are listed the constructs it includes. 
                                  For Mental health outcomes: mental wellbeing and mental health problems. 
                                  For Individual-level determinants: learning and development, healthy living, family support, social media, general health, and spirituality. 
                                  For Community-level determinants: participation, social support, trust, and safety. 
                                  For Structural-level determinants: equality, social inclusion, poverty and material deprivation, stigma discrimination and harassment, financial security/debt, physical environment, working life, and violence.")
  })
  
})
observeEvent(input$use, {
  output$text <- renderText({
    paste("<h2>Using the dashboard</h2>
      <h3>Structure of the dashboard</h3>
      The tabs along the top of the dashboard present the indicator data in different ways:
      <ul>
        <li>Summary of all the indicators:
          <ul>
            <li>Get a national overview in the <a class = 'data-link' onclick=\"$('li:eq(1) a').tab('show');\" role=\"button\"><b>Scotland profile</b></a> tab.</li>
            <li>Focus on a local area in the <a class = 'data-link' onclick=\"$('li:eq(2) a').tab('show');\" role=\"button\"><b>Local profile</b></a> tab.</li>
          </ul>
        </li>
        <li>Detail of time trends for a single indicator: 
          <ul>
            <li>Compare different areas in the <a class = 'data-link' onclick=\"$('li:eq(3) a').tab('show');\" role=\"button\"><b>Areas</b></a> tab.</li>
            <li>Look at inequalities by deprivation in the <a class = 'data-link' onclick=\"$('li:eq(4) a').tab('show');\" role=\"button\"><b>Deprivation</b></a> tab.</li>
            <li>Compare females and males in the <a class = 'data-link' onclick=\"$('li:eq(5) a').tab('show');\" role=\"button\"><b>Sex</b></a> tab.</li>
          </ul>
        </li>
        <li>See the data sources for each indicator on the <a class = 'data-link' onclick=\"$('li:eq(6) a').tab('show');\" role=\"button\"><b>Data sources</b></a> tab.</li>
      </ul>
      <h3>Interacting with the dashboard</h3>
      Select a tab from the top of the dashboard. 
      Each tab gives you options for customising the data presented.
      Make your selection(s) and the information presented will update automatically. 
      Additional option boxes will be shown, reflecting the data splits available for that indicator: geographical areas, deprivation level or sex.
      Where multiple geographical areas can be selected it is possible to deselect an area using the delete key.
      <br><br>
      <h3>Downloading the information</h3>      
      The national and local profiles can be downloaded as pdfs. The information presented on the other tabs can be downloaded as a Microsoft Excel spreadsheet. Simply click the download button.
      <br><br>"
    )
  })
  output$image <- renderUI({ "" })
  
})  

observeEvent(input$updates, {
  output$text <- renderText({
    paste("<h2>Dashboard updates</h2>
      The indicators presented in this dashboard are updated when new data become available. This occurs annually for most datasets. 
      The data were last updated in ",
          last_update,
          ". See the
      <a class = 'data-link' onclick=\"$('li:eq(6) a').tab('show');\" role=\"button\"><b> Data sources</b></a> tab 
      for details of when each indicator was last updated.
      <br><br>"
    )
  })
  
  output$image <- renderUI({ "" })
  
})

observeEvent(input$development, {
  output$text <- renderText({
    paste("<h2>How was the indicator set developed?</h2>
            In the mid 2000s, the Scottish Government commissioned NHS Health Scotland (now part of Public Health Scotland) to develop a set of mental health indicators. 
            The indicator set was to be used to:
            <ul>
            <li>Provide a baseline assessment of mental health and wellbeing in Scotland</li>
            <li>Monitor change over time</li>
            <li>Inform decision-making about priorities for action and resource allocation.</li>
            </ul>
            Development of the indicator set for adults began with four key ‘domains’, or overaching concepts. 
            One was concerned with mental health outcomes (mental health problems and mental wellbeing), and the remaining three included the contextual factors (risk factors and protective factors) that can influence mental health outcomes. 
            The contextual domains were individual, community and structural. Within each domain, important ‘constructs’ or themes were identified. Selection of the indicators was informed by evidence, expert opinion, theory, data availability, and policy. The indicators were to be measurable, informative, and relevant. 
            <br><br>The process of establishing the first adult mental health indicator set is described in 
            <a href=\"https://www.healthscotland.scot/media/2233/final-report-adult-mental-health-indicators-2007.pdf\" target=\"_blank\"><b>NHS Health Scotland’s 2007 report</b></a>. 
            National data for these indicators were published by the Scottish Public Health Observatory (ScotPHO) in its 
            <a href=\"https://www.scotpho.org.uk/media/1227/scotpho090227_mhadults2009_rep.pdf\" target=\"_blank\"><b>2009 report</b></a> and 
            <a href=\"https://www.scotpho.org.uk/media/1183/scotpho121019_mhadults2012_fullreport.pdf\" target=\"_blank\"><b>2012 report</b></a>. 
            More recently, data for selected indicators have been published on the 
            <a href=\"https://www.scotpho.org.uk/comparative-health/profiles/online-profiles-tool/\" target=\"_blank\"><b>ScotPHO Online Profiles Tool</b></a>.  
            <br><br>The adult mental health indicator set was reviewed and revised in 2022, to ensure its continued relevance. 
            The work – involving an evidence review, expert input, and a community workshop – is described in the 
            <a href=\"https://publichealthscotland.scot/our-areas-of-work/health-and-wellbeing/prevention-of-mental-ill-health-and-improved-wellbeing/mental-health-indicators/adult-mental-health-indicators/\" target=\"_blank\"><b>resources</b></a>
            on our website. This dashboard presents data on the revised indicator set.
            <br><br>It is important to note that mental health is an evolving concept. The current indicator set should not be seen as a definitive means of measuring mental health and wellbeing in Scotland, 
            but rather a pragmatic reflection of what can be measured within current constraints of theory, evidence, and data availability.	
            <br><br>")
    
  })
  output$image <- renderUI({ "" })
  
})  
observeEvent(input$info, {
  output$text <- renderText({
    paste("<h2>Where can I find further information?</h2> 
      <a href=\"https://publichealthscotland.scot/our-areas-of-work/health-and-wellbeing/prevention-of-mental-ill-health-and-improved-wellbeing/mental-health-indicators/adult-mental-health-indicators/\" target=\"_blank\"><b>The adult mental health indicator set</b></a>
      <br><a href=\"https://publichealthscotland.scot/publications/mental-health-indicator-process-paper/\" target=\"_blank\"><b>How the indicator set was developed</b></a>
      <br><a href=\"https://www.publichealthscotland.scot/our-areas-of-work/public-mental-health-and-wellbeing/prevention-of-mental-ill-health-and-improved-wellbeing/mental-health-indicators/overview/\" target=\"_blank\"><b>Public Health Scotland's mental health indicator pages</b></a>
      <br><a href=\"https://www.scotpho.org.uk/comparative-health/profiles/online-profiles-tool/\" target=\"_blank\"><b>The ScotPHO Online Profiles Tool</b></a>
      <br><a href=\"https://publichealthscotland.scot/our-areas-of-work/health-and-wellbeing/prevention-of-mental-ill-health-and-improved-wellbeing/mental-health-indicators/children-and-young-people-mental-health-indicators/\" target=\"_blank\"><b>Children and Young People's Mental Health Indicators</b></a>
      <br><a href=\"https://www.healthscotland.scot/health-topics/mental-health-and-wellbeing/overview-of-mental-health-and-wellbeing\" target=\"_blank\"><b>NHS Health Scotland Mental Health and Wellbeing pages (archived)</b></a>
      <br><br><h2>Links to other resources</h2> 
      <a href=\"https://www.gov.scot/publications/mental-health-wellbeing-strategy/\" target=\"_blank\"><b>Scottish Government and COSLA's Mental Health and Wellbeing Strategy (2023)</b></a>
      <br><a href=\"https://publichealthscotland.scot/publications/mental-health-quality-indicator-profile/mental-health-quality-indicator-profile-28-november-2023/\" target=\"_blank\"><b>PHS Mental health Quality Indicator Profile (2023)</b></a>
      <br><a href=\"https://publichealthscotland.scot/our-areas-of-work/health-and-wellbeing/public-health-scotland-s-approach-to-mental-health/information-for-the-public/\" target=\"_blank\"><b>Sources of support</b></a>
      <br><br>")
  })
  output$image <- renderUI({ "" })
  
})  
observeEvent(input$glossary, {
  output$text <- renderText({
    paste("<h2>Glossary of terms used in the dashboard</h2> 
              <table>
              <tr>
              <th style=\"width:30%\">Term</th>
              <th>Meaning</th>
              </tr>
              <tr>
              <td><b>Absolute Inequality</td>
              <td>The absolute inequality gap in the population. We measure this using the Slope Index of Inequality (SII), which takes into consideration all the deprivation groups and the population size of each. 
              See <b><a href=\"https://www.scotpho.org.uk/methods-and-data/measuring-health-inequalities/#siirii\">the ScotPHO website</a></b> for more information. </td>
              </tr>
              <tr>
              <td><b>Adult</td>
              <td>We use 16 years and over for most indicators. Exceptions are indicated in the Data sources tab.</td>
              </tr>
              <tr>
              <td><b>Confidence Interval</td>
              <td>An interval that is expected to contain the true value being estimated. We use the 95% confidence interval, which is expected to contain the true value in 95 out of 100 cases.</td>
              </tr>
              <tr>
              <td><b>Construct</td>
              <td>Categories of related indicators on a common theme.</td>
              </tr>
              <tr>
              <td><b>Crude Rate</td>
              <td>A rate (number of cases divided by total population) that does not account for differing age- and sex-structures between the populations being compared.</td>
              </tr>
              <tr>
              <td><b>Domain</td>
              <td>The adult mental health indicators are grouped into four high-level, overarching concepts, or 'domains': mental health outcomes, individual determinants, community determinants, and structural determinants.</td>
              </tr>
              <tr>
              <td><b>Indicator</td>
              <td>A specific measure capturing relevant information about the prevalence or scale of a specific health outcome or determinant.
              </td>
              </tr>
              <tr>
              <td><b>Quintile</td>
              <td>Fifths of the Scottish Index of Multiple Deprivation <b><a href=\"https://www.gov.scot/collections/scottish-index-of-multiple-deprivation-2020/\">(SIMD)</a></b>, from the 1st quintile (most deprived 20% of Scottish datazones) to the 5th quintile (the least deprived 20% of Scottish datazones).</td>
              </tr>
              <tr>
              <td><b>Relative Inequality</td>
              <td>The relative inequality gap in the population. We measure this using the Relative Index of Inequality (RII), which takes into consideration all the deprivation groups and the population size of each. 
              See <b><a href=\"https://www.scotpho.org.uk/methods-and-data/measuring-health-inequalities/#siirii\">the ScotPHO website</a></b> for more information.</td>
              </tr>
              <tr>
              <td><b>SIMD</td>
              <td>An acronym for the <b><a href=\"https://www.gov.scot/collections/scottish-index-of-multiple-deprivation-2020/\">'Scottish Index of Multiple Deprivation'</a></b>. A measure of area-level deprivation.</td>
              </tr>
              <tr>
              <td><b>Standardised Rate</td>
              <td>A rate that takes account of differing age- and sex-structures between populations, to aid comparison.</td>
              </tr>
              
              </table>
            
            ")
  })
  output$image <- renderUI({ "" })
  
})  

observeEvent(input$contact, {
  output$text <- renderText({
    paste("<h2>Contact us</h2> 
      If you have any questions or feedback regarding this dashboard or any information contained within it, please contact us at: 
              # INSERT EMAIL ADDRESS, and we will be happy to help.
            <br><br>")  
  })
  output$image <- renderUI({ "" })
  
})
observeEvent(input$accessibility, {
  output$text <- renderText({
    paste("<h2>Accessibility</h2>
      This website is run by
      <a href=\"https://www.publichealthscotland.scot/\" target=\"_blank\"><b>Public Health Scotland</b></a>
      , Scotland's national organisation for public health. 
      Public Health Scotland is committed to making its website accessible, 
      in accordance with the Public Sector Bodies (Websites and Mobile Applications) (No. 2) Accessibility Regulations 2018.
      <br><br><a href=\"https://mcmw.abilitynet.org.uk/\" target=\"_blank\"><b>AbilityNet (external website)</b></a>            
      has advice on making your device easier to use if you have a disability.
      <br><br>
      <h3>How accessible is this website?</h3>
      This site has not yet been evaluated against WCAG 2.1 level AA.
      <br><br>
      <h3>Reporting any accessibility problems with this website</h3>
      If you wish to contact us about any accessibility issues you encounter on this site, 
      please email phs.alcohol@phs.scot. # CHANGE EMAIL ADDRESS
      <br><br>
      <h3>Enforcement procedure</h3>
      The Equality and Human Rights Commission (EHRC) is responsible for enforcing the Public Sector Bodies 
      (Websites and Mobile Applications) (No. 2) Accessibility Regulations 2018 (the ‘accessibility regulations’). 
      If you’re not happy with how we respond to your complaint, contact the 
      <a href=\"https://www.equalityadvisoryservice.com/\" target=\"_blank\"><b>Equality Advisory and Support Service (EASS) (external website).</b></a>
      <br><br>")  
  })
  output$image <- renderUI({ "" })
  
})  

