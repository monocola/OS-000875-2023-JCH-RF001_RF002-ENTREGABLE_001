import { NgModule, CUSTOM_ELEMENTS_SCHEMA } from '@angular/core';
import { CommonModule, CurrencyPipe } from '@angular/common';

import { EvaluacionCurricularRoutingModule } from './evaluacion-curricular-routing.module';
import { EvaluacionCurricularComponent } from './evaluacion-curricular.component';

import { MatDividerModule } from '@angular/material/divider';
import { MatStepperModule } from '@angular/material/stepper';
import { MatSlideToggleModule } from '@angular/material/slide-toggle';
import { MatDialogModule } from '@angular/material/dialog';
import { MatIconModule } from '@angular/material/icon';

import {
  NbButtonModule,
  NbDatepickerModule,
  NbFormFieldModule,
  NbIconModule,
  NbInputModule,
  NbCardModule,
  NbPopoverModule,
  NbSelectModule,
  NbAutocompleteModule,
  NbRadioModule,
  NbCheckboxModule
} from '@nebular/theme';

import { FormsModule, ReactiveFormsModule } from '@angular/forms';
import { CommonComponentsModule } from './../../@common-components/common-components.module';

import { EvaluacionDetalleComponent } from './evaluacion-detalle/evaluacion-detalle.component';
import { EvaluacionPerfilDetalleComponent } from './evaluacion-perfil-detalle/evaluacion-perfil-detalle.component';

import { ModalAlertConfirmComponent } from './modal-alert-confirm/modal-alert-confirm.component';
import { ModalVerDocumentoComponent } from './modal-ver-documento/modal-ver-documento.component';
import { MatSelectModule } from '@angular/material/select';
import { ModalBonificacionConfirmComponent } from './modal-bonificacion-confirm/modal-bonificacion-confirm.component';
import { FillDigitPipe } from '../../@common-components/customize-pipe/fill-digit.pipe';
import { TableEvaluacionDetalleComponent } from './evaluacion-detalle/table-evaluacion-detalle/table-evaluacion-detalle.component';
import { MatTableModule } from '@angular/material/table';
import { MatRippleModule } from '@angular/material/core';
import { MatSortModule } from '@angular/material/sort';
import { MatPaginatorModule } from '@angular/material/paginator';
import { MatButtonModule } from '@angular/material/button';
import { ModalRedereciComponent } from './evaluacion-detalle/modal-redereci/modal-redereci.component';


@NgModule({
  declarations: [
    EvaluacionCurricularComponent,
    EvaluacionDetalleComponent,
    EvaluacionPerfilDetalleComponent,
    ModalAlertConfirmComponent,
    ModalVerDocumentoComponent,
    ModalBonificacionConfirmComponent,
    FillDigitPipe,
    TableEvaluacionDetalleComponent,
    ModalRedereciComponent,
  ],
  imports: [
    CommonModule,
    EvaluacionCurricularRoutingModule,
    MatStepperModule,
    MatDialogModule,
    MatIconModule,
    FormsModule,
    ReactiveFormsModule,
    CommonComponentsModule,
    NbButtonModule,
    NbDatepickerModule,
    NbFormFieldModule,
    NbIconModule,
    NbInputModule,
    NbCardModule,
    NbPopoverModule,
    NbSelectModule,
    NbAutocompleteModule,
    NbRadioModule,
    MatSelectModule,
    NbCheckboxModule,

    MatSlideToggleModule,
    MatButtonModule,

    MatDividerModule,
    MatPaginatorModule,
    MatTableModule,
    MatSortModule,
    MatRippleModule,
  ],
  providers: [CurrencyPipe,FillDigitPipe],
  schemas: [CUSTOM_ELEMENTS_SCHEMA],
})
export class EvaluacionCurricularModule {}
