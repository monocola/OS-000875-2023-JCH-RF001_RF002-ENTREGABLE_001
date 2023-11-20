import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';
import { BuscarDocumentoComponent } from './buscar-documento/buscar-documento.component';
import {
  NbButtonModule,
  NbDatepickerModule,
  NbFormFieldModule,
  NbIconModule,
  NbInputModule,
  NbSelectModule,
  NbTreeGridModule
} from '@nebular/theme';
import { MatAutocompleteModule } from '@angular/material/autocomplete';
import { FormsModule, ReactiveFormsModule } from '@angular/forms';
import { CronogramasComponent } from './cronogramas/cronogramas.component';
import { CommonComponentsModule } from 'src/app/@presentation/@common-components/common-components.module';
import { DragDropModule } from '@angular/cdk/drag-drop';
import { MatButtonModule } from '@angular/material/button';
import { MatChipsModule } from '@angular/material/chips';
import { MatRadioModule } from '@angular/material/radio';
import { RouterModule } from '@angular/router';
import { ComponentsPerfilesModule } from '../../perfiles/components/components.perfiles.module';
import { EditarCronogramaComponent } from './editar-cronograma/editar-cronograma.component';
import { BottomDivBasesComponent } from './bottom-div-bases/bottom-div-bases.component';
import { ModalAddObservacionComponent } from './modal-add-observacion/modal-add-observacion.component';
import { ObservacionBaseDivComponent } from './observacion-base-div/observacion-base-div.component';
import { MatDialogModule } from '@angular/material/dialog';
import { ModalNotificacionComponent } from './modal-notificacion/modal-notificacion.component';
import { BaseRevisadaModalComponent } from './base-revisada-modal/base-revisada-modal.component';
import { ModalBaseObsLevantadasComponent } from './modal-base-obs-levantadas/modal-base-obs-levantadas.component';
import { MatRippleModule } from '@angular/material/core';
import { BaseArraysFieldComponent } from './base-arrays-field/base-arrays-field.component';
import { BaseAutoArraysFieldComponent } from './base-auto-arrays-field/base-auto-arrays-field.component';
import {
  NgxMatDatetimePickerModule,
  NgxMatTimepickerModule,
  NgxMatNativeDateModule,
  NgxMatDateFormats,
  NGX_MAT_DATE_FORMATS,
} from '@angular-material-components/datetime-picker';
import { MatDatepickerModule } from '@angular/material/datepicker';
import { MatNativeDateModule } from '@angular/material/core';
import { TableCronogramaComponent } from './tabla-cronograma/table-cronograma.component';

const INTL_DATE_INPUT_FORMAT = {
  year: 'numeric',
  month: 'numeric',
  day: 'numeric',
  hourCycle: 'h24',
  hour: '2-digit',
  minute: '2-digit',
};

const MAT_DATE_FORMATS: NgxMatDateFormats = {
  parse: {
    dateInput: INTL_DATE_INPUT_FORMAT,
  },
  display: {
    dateInput: INTL_DATE_INPUT_FORMAT,
    monthYearLabel: { year: 'numeric', month: 'short' },
    dateA11yLabel: { year: 'numeric', month: 'long', day: 'numeric' },
    monthYearA11yLabel: { year: 'numeric', month: 'long' },
  },
};

const COMPONENTS = [
  BuscarDocumentoComponent,
  CronogramasComponent,
  TableCronogramaComponent,
  EditarCronogramaComponent,
  BottomDivBasesComponent,
  ModalAddObservacionComponent,
  ObservacionBaseDivComponent,
  ModalNotificacionComponent,
  BaseRevisadaModalComponent,
  ModalBaseObsLevantadasComponent,
  BaseArraysFieldComponent,
  BaseAutoArraysFieldComponent,
];
@NgModule({
  imports: [
    MatNativeDateModule,
    MatDatepickerModule,
    NgxMatDatetimePickerModule,
    NgxMatTimepickerModule,
    NgxMatNativeDateModule,
    CommonModule,
    NbFormFieldModule,
    NbInputModule,
    ReactiveFormsModule,
    MatAutocompleteModule,
    NbIconModule,
    CommonComponentsModule,
    ComponentsPerfilesModule,
    FormsModule,
    NbSelectModule,
    MatRadioModule,
    NbButtonModule,
    MatChipsModule,
    NbTreeGridModule,
    DragDropModule,
    MatButtonModule,
    RouterModule,
    NbDatepickerModule,
    MatDialogModule,
    MatRippleModule,
  ],
  declarations: [...COMPONENTS],
  exports: [...COMPONENTS],
  providers: [{ provide: NGX_MAT_DATE_FORMATS, useValue: MAT_DATE_FORMATS }],
})
export class BaseComponentsModule {}
