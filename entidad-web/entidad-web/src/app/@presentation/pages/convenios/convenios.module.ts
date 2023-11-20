import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';

import { ConveniosRoutingModule } from './convenios-routing.module';
import { FormsModule, ReactiveFormsModule } from '@angular/forms';
import { MatRippleModule } from '@angular/material/core';
import { MatDividerModule } from '@angular/material/divider';
import { MatFormFieldModule } from '@angular/material/form-field';
import { MatInputModule } from '@angular/material/input';
import {
  NbButtonModule,
  NbDatepickerModule,
  NbTimepickerModule,
  NbFormFieldModule,
  NbIconModule,
  NbInputModule,
} from '@nebular/theme';
import { CommonComponentsModule } from '../../@common-components/common-components.module';
import { MatIconModule } from '@angular/material/icon';
import { MatButtonModule } from '@angular/material/button';
import { MatPaginatorModule } from '@angular/material/paginator';
import { MatTableModule } from '@angular/material/table';
import { MatSortModule } from '@angular/material/sort';
import { ConveniosComponent } from './convenios.component';
import { ElaborarConvenioComponent } from './elaborar-convenio/elaborar-convenio.component';
import { ModalConvenioComponent } from './modal-convenio/modal-convenio.component';
import { ModalConvenioEntidadComponent } from './modal-convenio-entidad/modal-convenio-entidad.component';
import { ModalDataEntidadComponent } from './modal-data-entidad/modal-data-entidad.component';

@NgModule({
  declarations: [
    ConveniosComponent,
    ElaborarConvenioComponent,
    ModalConvenioComponent,
    ModalConvenioEntidadComponent,
    ModalDataEntidadComponent,
  ],
  imports: [
    CommonModule,
    MatTableModule,
    ConveniosRoutingModule,
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
    MatSortModule,
    NbTimepickerModule,
  ],
})
export class ConveniosModule {}
