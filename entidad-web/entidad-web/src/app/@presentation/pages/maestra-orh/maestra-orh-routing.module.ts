import { NgModule } from '@angular/core';
import { Routes, RouterModule } from '@angular/router';
import { MaestraOrhComponent } from './maestra-orh.component';

const routes: Routes = [{ path: '', component: MaestraOrhComponent }];

@NgModule({
  imports: [RouterModule.forChild(routes)],
  exports: [RouterModule],
})
export class MaestraOrhRoutingModule {}
