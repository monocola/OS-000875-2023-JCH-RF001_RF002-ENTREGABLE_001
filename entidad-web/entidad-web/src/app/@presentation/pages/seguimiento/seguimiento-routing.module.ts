import { NgModule } from '@angular/core';
import { Routes, RouterModule } from '@angular/router';
import { SeguimientoComponent } from './seguimiento.component';
import { CronogramaComponent } from './cronograma/cronograma.component';

const routes: Routes = [
  { path: '', component: SeguimientoComponent },
  { path: 'cronograma', component: CronogramaComponent },

];

@NgModule({
  imports: [RouterModule.forChild(routes)],
  exports: [RouterModule],
})
export class SeguimientoRoutingModule { }
