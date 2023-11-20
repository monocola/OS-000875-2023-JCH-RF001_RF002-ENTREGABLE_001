import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';
import { ThemeModule } from 'src/app/@presentation/@theme/theme.module';
import {
  NbAutocompleteModule,
  NbButtonModule,
  NbDatepickerModule,
  NbFormFieldModule,
  NbIconModule,
  NbInputModule,
  NbLayoutModule,
  NbMenuModule,
  NbRadioModule,
  NbSelectModule,
} from '@nebular/theme';
import { FormsModule, ReactiveFormsModule } from '@angular/forms';
import { MatMenuModule } from '@angular/material/menu';
import { DetailComponent } from './detail/detail.component';
import { ModalVerificationComponent } from './detail/modal-verification/modal-verification.component';

import { MatDialogModule } from '@angular/material/dialog';
import { MatListModule } from '@angular/material/list';
import { NgxTrimDirectiveModule } from 'ngx-trim-directive';
import { MatDividerModule } from '@angular/material/divider';
import { CommonComponentsModule } from '../../@common-components/common-components.module';
import { AdministradorSolicitudesComponent } from './administrador-solicitudes.component';
import { AdministradorSolicitudesRoutingModule } from './administrador-solicitudes-routing.module';

@NgModule({
  declarations: [
    AdministradorSolicitudesComponent,
    DetailComponent,
    ModalVerificationComponent,
  ],
  imports: [
    CommonModule,
    AdministradorSolicitudesRoutingModule,
    ThemeModule,
    NbMenuModule,
    NbButtonModule,
    NbIconModule,
    CommonComponentsModule,
    NbFormFieldModule,
    FormsModule,
    NbSelectModule,
    NbDatepickerModule,
    NbAutocompleteModule,
    NbLayoutModule,
    ReactiveFormsModule,
    MatMenuModule,
    NbInputModule,
    MatDialogModule,
    NbRadioModule,
    MatListModule,
    NgxTrimDirectiveModule,
    MatDividerModule,
  ],
})
export class AdministradorSolicitudesModule {}
