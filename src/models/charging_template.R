# Template for the charging data
templateCharging <- function(chargedkWh, efficiency, start, end) {
  sprintf(
    '<table><tbody>
    <tr><td colspan="3"><em>%s</em></td></tr>
    <tr><td colspan="3"><em>%s</em></td></tr>
    <tr><td colspan="3"><em>%s</em></td></tr>
    <tr><td colspan="3"><em>%s</em></td></tr>
    </tbody></table>',
    chargedkWh, efficiency, start, end
  )
}