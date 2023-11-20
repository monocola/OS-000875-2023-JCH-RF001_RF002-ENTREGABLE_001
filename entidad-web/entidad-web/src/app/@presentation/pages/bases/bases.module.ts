import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';

import { BasesRoutingModule } from './bases-routing.module';
import { BasesComponent } from './bases.component';
import { CreacionBaseComponent } from './creacion-base/creacion-base.component';
import { CommonComponentsModule } from '../../@common-components/common-components.module';
import { SelectBaseComponent } from './select-base/select-base.component';
import { Step1Component } from './creacion-base/step1/step1.component';
import { Step2Component } from './creacion-base/step2/step2.component';
import { Step3Component } from './creacion-base/step3/step3.component';
import { Step4Component } from './creacion-base/step4/step4.component';
import { Step5Component } from './creacion-base/step5/step5.component';
import { Step6Component } from './creacion-base/step6/step6.component';
import { BaseComponentsModule } from './components/base-components.module';
import { AngularResizedEventModule } from 'angular-resize-event';
import { MatStepperModule } from '@angular/material/stepper';
import { ComponentsPerfilesModule } from '../perfiles/components/components.perfiles.module';
import { MatDividerModule } from '@angular/material/divider';
import { FormsModule, ReactiveFormsModule } from '@angular/forms';
import {
  NbButtonModule,
  NbDatepickerModule,
  NbFormFieldModule,
  NbIconModule,
  NbInputModule,
  NbPopoverModule,
  NbSelectModule,
  NbTimepickerModule,
  NB_TIME_PICKER_CONFIG,
  NbAutocompleteModule,
} from '@nebular/theme';
import { MatFormFieldModule } from '@angular/material/form-field';
import { MatIconModule } from '@angular/material/icon';
import { MatInputModule } from '@angular/material/input';
import { TablaBaseComponent } from './tabla-base/tabla-base.component';
import { MatPaginatorModule } from '@angular/material/paginator';
import { MatTableModule } from '@angular/material/table';
import { MatSortModule } from '@angular/material/sort';
import { MatButtonModule } from '@angular/material/button';
import { MatCheckboxModule } from '@angular/material/checkbox';
import { TableVacantesPerfilComponent } from './creacion-base/step2/table-vacantes-perfil/table-vacantes-perfil.component';
import { ModalHorariosVacantesComponent } from './creacion-base/step2/modal-horarios-vacantes/modal-horarios-vacantes.component';
import { MatRippleModule } from '@angular/material/core';
import { MatDialogModule } from '@angular/material/dialog';
import { MatAutocompleteModule } from '@angular/material/autocomplete';
import {
  NgxMatDatetimePickerModule,
  NgxMatTimepickerModule,
  NgxMatNativeDateModule,
  NgxMatDateFormats,
  NGX_MAT_DATE_FORMATS,
} from '@angular-material-components/datetime-picker';
import { MatDatepickerModule } from '@angular/material/datepicker';
import { MatNativeDateModule } from '@angular/material/core';

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

@NgModule({
  declarations: [
    BasesComponent,
    CreacionBaseComponent,
    SelectBaseComponent,
    Step1Component,
    Step2Component,
    Step3Component,
    Step4Component,
    Step5Component,
    Step6Component,
    TablaBaseComponent,
    TableVacantesPerfilComponent,
    ModalHorariosVacantesComponent,
  ],
  imports: [
    MatAutocompleteModule,
    NgxMatDatetimePickerModule,
    NgxMatTimepickerModule,
    NgxMatNativeDateModule,
    MatDatepickerModule,
    MatNativeDateModule,
    CommonModule,
    BasesRoutingModule,
    CommonComponentsModule,
    BaseComponentsModule,
    AngularResizedEventModule,
    MatStepperModule,
    ComponentsPerfilesModule,
    MatDividerModule,
    ReactiveFormsModule,
    FormsModule,
    NbButtonModule,
    NbFormFieldModule,
    NbDatepickerModule,
    NbIconModule,
    NbInputModule,
    NbPopoverModule,
    MatFormFieldModule,
    MatIconModule,
    MatInputModule,
    MatPaginatorModule,
    MatTableModule,
    MatSortModule,
    MatButtonModule,
    MatCheckboxModule,
    NbTimepickerModule,
    NbSelectModule,
    MatRippleModule,
    MatDialogModule,
    NbAutocompleteModule,
  ],
  providers: [
    {
      provide: NB_TIME_PICKER_CONFIG,
      useValue: {},
    },
    { provide: NGX_MAT_DATE_FORMATS, useValue: MAT_DATE_FORMATS },
  ],
})
export class BasesModule {}
