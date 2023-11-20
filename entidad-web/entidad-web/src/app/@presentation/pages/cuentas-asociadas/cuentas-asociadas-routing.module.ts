import { NgModule } from '@angular/core';
import { Routes, RouterModule } from '@angular/router';
import { CuentasAsociadasComponent } from './cuentas-asociadas.component';

const routes: Routes = [{ path: '', component: CuentasAsociadasComponent }];

@NgModule({
  imports: [RouterModule.forChild(routes)],
  exports: [RouterModule],
})
export class CuentasAsociadasRoutingModule {}
