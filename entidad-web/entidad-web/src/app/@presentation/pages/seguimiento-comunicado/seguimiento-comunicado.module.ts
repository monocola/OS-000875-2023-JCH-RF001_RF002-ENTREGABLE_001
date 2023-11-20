import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';

import { SeguimientoComunicadoRoutingModule } from './seguimiento-comunicado-routing.module';
import { SeguimientoComunicadoComponent } from './seguimiento-comunicado.component';
import { FormsModule, ReactiveFormsModule } from '@angular/forms';
import { MatRippleModule } from '@angular/material/core';
import { MatDividerModule } from '@angular/material/divider';
import { MatFormFieldModule } from '@angular/material/form-field';
import { MatInputModule } from '@angular/material/input';
import {
  NbButtonModule,
  NbDatepickerModule,
  NbFormFieldModule,
  NbIconModule,
  NbInputModule,
} from '@nebular/theme';
import { CommonComponentsModule } from '../../@common-components/common-components.module';
import { MatIconModule } from '@angular/material/icon';
import { MatButtonModule } from '@angular/material/button';
import { MatPaginatorModule } from '@angular/material/paginator';
import { TablaComunicadoComponent } from './tabla-comunicado/tabla-comunicado.component';
import { MatTableModule } from '@angular/material/table';
import { ModalComunicadoComponent } from './modal-comunicado/modal-comunicado.component';
import { RegistroComunicadoComponent } from './registro-comunicado/registro-comunicado.component';
import { ModalObservarComponent } from './modal-observar/modal-observar.component';
import { ModalPublicarComponent } from './modal-publicar/modal-publicar.component';
import { ModalAprobarComponent } from './modal-aprobar/modal-aprobar.component';
import { MatSortModule } from '@angular/material/sort';

@NgModule({
  declarations: [SeguimientoComunicadoComponent, TablaComunicadoComponent, ModalComunicadoComponent, RegistroComunicadoComponent, ModalObservarComponent, ModalPublicarComponent, ModalAprobarComponent],
  imports: [
    CommonModule,
    MatTableModule,
    SeguimientoComunicadoRoutingModule,
    MatDividerModule,
    MatRippleModule,
    NbButtonModule,
    CommonComponentsModule,
    MatFormFieldModule,
    MatInputModule,
    FormsModule,
    ReactiveFormsModule,
    NbIconModule,
    NbInputModule,
    NbFormFieldModule,
    NbDatepickerModule,
    MatIconModule,
    MatButtonModule,
    MatPaginatorModule,
    MatSortModule
  ]
})
export class SeguimientoComunicadoModule {}
