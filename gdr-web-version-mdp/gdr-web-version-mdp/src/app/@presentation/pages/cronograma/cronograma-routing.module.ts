import { RouterModule, Routes } from '@angular/router';
import { CronogramaComponent } from './cronograma.component';
import { NgModule } from '@angular/core';
import { HistorialCronogramaComponent } from './historial-cronograma/historial-cronograma.component';

const routes: Routes = [
  { path: '', component: CronogramaComponent },
  { path: 'historial', component: HistorialCronogramaComponent }
];

@NgModule({
  imports: [RouterModule.forChild(routes)],
  exports: [RouterModule],
})

export class CronogramaRoutingModule {}
