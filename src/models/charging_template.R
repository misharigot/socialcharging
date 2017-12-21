# Template for the charging data
templateCharging <- function(efficiencyClass, chargedkWh, start, end) {
  sprintf(
    '<div class="efficiency %s">
    <span style="display:block">%s</span>
    <span style="display:block">%s</span>
    <span style="display:block">%s</span>
    </div>',
    efficiencyClass, chargedkWh, start, end
  )
}