import { NgModule } from '@angular/core';
import { RouterModule, Routes } from '@angular/router';
import { CanDeactivateGuard } from 'src/app/@data/guards/can-deactivate.guard';
import {
  CoordinadorGuard,
  GestorOrhGuard,
} from 'src/app/@data/guards/index.guard';
import { MasterGuard } from 'src/app/@data/guards/master.guard';
import { BasesComponent } from './bases.component';
import { CreacionBaseComponent } from './creacion-base/creacion-base.component';
import { SelectBaseComponent } from './select-base/select-base.component';
import { SuperAdminEntidadGuard } from 'src/app/@data/guards/super.admin.entidad.guard';
// canActivate: [MasterGuard],
// data: {
//   guards: [AdminEntidadGuard, GestorOrhGuard],
//   title: 'Gestiones de bases - Servir Talento Perú',
//   guardsRelation: 'OR',
// },
const routes: Routes = [
  { path: '', component: BasesComponent },
  {
    path: 'elige',
    component: SelectBaseComponent,
    canActivate: [MasterGuard],
    data: {
      guards: [GestorOrhGuard, CoordinadorGuard, SuperAdminEntidadGuard ],
      title: 'Elige la base a crear - Servir Talento Perú',
      guardsRelation: 'OR',
    },
  },
  {
    path: 'creacion',
    component: CreacionBaseComponent,
    canActivate: [MasterGuard],
    data: {
      guards: [GestorOrhGuard, CoordinadorGuard, SuperAdminEntidadGuard],
      title: 'Gestión de bases - Servir Talento Perú',
      guardsRelation: 'OR',
    },
    canDeactivate: [CanDeactivateGuard],
  },
];

@NgModule({
  imports: [RouterModule.forChild(routes)],
  exports: [RouterModule],
})
export class BasesRoutingModule {}
