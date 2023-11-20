import { NgModule } from '@angular/core';
import { Routes, RouterModule } from '@angular/router';
import { ReporteServirComponent } from './reporte-servir.component';

const routes: Routes = [{ path: '', component: ReporteServirComponent }];

@NgModule({
  imports: [RouterModule.forChild(routes)],
  exports: [RouterModule],
})
export class ReporteServirRoutingModule {}
