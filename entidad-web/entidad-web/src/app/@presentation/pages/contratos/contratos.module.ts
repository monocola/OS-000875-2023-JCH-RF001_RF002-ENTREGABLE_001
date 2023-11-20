import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';
import { ContratosRoutingModule } from './contratos-routing.module';
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
  NbCheckboxModule,
} from '@nebular/theme';
import { CommonComponentsModule } from '../../@common-components/common-components.module';
import { ElaborarContratoComponent } from './elaborar-contrato/elaborar-contrato.component';
import { MatIconModule } from '@angular/material/icon';
import { MatButtonModule } from '@angular/material/button';
import { MatPaginatorModule } from '@angular/material/paginator';
import { MatTableModule } from '@angular/material/table';
import { MatSortModule } from '@angular/material/sort';
import { ContratosComponent } from './contratos.component';
import { ModalContratoComponent } from './modal-contrato/modal-contrato.component';
import { ModalObservarComponent } from './modal-observar/modal-observar.component';
import { ModalEntidadComponent } from './modal-entidad/modal-entidad.component';
import { ModalPostulanteComponent } from './modal-postulante/modal-postulante.component';
import { ModalAnularContratoComponent } from './modal-anular-contrato/modal-anular-contrato.component';
import { TablaModalContratosComponent } from './tabla-modal-contratos/tabla-modal-contratos.component';
import { ModalDescargaComponent } from './modal-descarga/modal-descarga.component';

@NgModule({
  declarations: [
    ContratosComponent,
    ModalContratoComponent,
    ElaborarContratoComponent,
    ModalObservarComponent,
    ModalEntidadComponent,
    ModalPostulanteComponent,
    ModalAnularContratoComponent,
    TablaModalContratosComponent,
    ModalDescargaComponent,
  ],
  imports: [
    CommonModule,
    MatTableModule,
    ContratosRoutingModule,
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
    NbCheckboxModule,
    MatIconModule,
    MatButtonModule,
    MatPaginatorModule,
    MatSortModule,
  ],
})
export class ContratosModule {}
