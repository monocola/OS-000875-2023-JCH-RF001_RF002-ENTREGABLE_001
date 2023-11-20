import { NgModule } from '@angular/core';
import { Routes, RouterModule } from '@angular/router';
import { TablaMaestraComponent } from './maestra-servir.component';

const routes: Routes = [
  { path: '', component: TablaMaestraComponent },
];

@NgModule({
  imports: [RouterModule.forChild(routes)],
  exports: [RouterModule],
})
export class TablaMaestraRoutingModule {}
