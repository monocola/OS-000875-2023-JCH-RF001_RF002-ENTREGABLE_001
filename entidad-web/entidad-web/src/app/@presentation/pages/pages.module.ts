import { NgModule } from '@angular/core';
import { ThemeModule } from '../@theme/theme.module';
import { PagesRoutingModule } from './pages-routing.module';
import { PagesComponentsModule } from './components/components.module';

import {
  NbActionsModule,
  NbButtonModule,
  NbCardModule,
  NbLayoutModule,
  NbSelectModule,
  NbSidebarModule,
  NbIconModule
} from '@nebular/theme';

// Components
import { PagesComponent } from './pages.component';
import { DefaultPageComponent } from './default-page/default-page.component';
import { NotBuildedComponent } from './miscellaneous/not-builded/not-builded.component';
import { MatDialogModule } from '@angular/material/dialog';
import { CanDeactivateGuard } from 'src/app/@data/guards/can-deactivate.guard';
import { MasterGuard } from 'src/app/@data/guards/master.guard';
import { ModalRegistroConocimientoComponent } from './conocimiento/modal-registro-conocimiento/modal-registro-conocimiento.component';
import { ReactiveFormsModule } from '@angular/forms';

@NgModule({
  imports: [
    PagesRoutingModule,
    ThemeModule,
    NbLayoutModule,
    NbSidebarModule,
    PagesComponentsModule,
    MatDialogModule,
    ReactiveFormsModule,
    NbCardModule,
    NbActionsModule,
    NbSelectModule,
    NbButtonModule,
    NbIconModule
  ],
  declarations: [PagesComponent, DefaultPageComponent, NotBuildedComponent, ModalRegistroConocimientoComponent],
  providers: [CanDeactivateGuard, MasterGuard],
})
export class PagesModule {}
