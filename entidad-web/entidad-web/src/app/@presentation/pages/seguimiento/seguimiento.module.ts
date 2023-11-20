import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';
import { NbCalendarRangeModule } from '@nebular/theme';
import { SeguimientoRoutingModule } from './seguimiento-routing.module';
import { SeguimientoComponent } from './seguimiento.component';
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
  NbTreeGridModule,
} from '@nebular/theme';
import { CommonComponentsModule } from '../../@common-components/common-components.module';
import { CronogramaComponent } from './cronograma/cronograma.component';
import { FullCalendarModule } from '@fullcalendar/angular';
import { ModalRegistroComponent } from './cronograma/modal-registro/modal-registro.component';
import { MatDatepickerModule } from '@angular/material/datepicker';
import {
  NgxMatDatetimePickerModule,
  NgxMatTimepickerModule,
  NgxMatNativeDateModule,
  NgxMatDateFormats,
  NGX_MAT_DATE_FORMATS,
} from '@angular-material-components/datetime-picker';
import { NgxMatMomentModule } from '@angular-material-components/moment-adapter';
import { MatNativeDateModule } from '@angular/material/core';
import { MatButtonModule } from '@angular/material/button';
import { BaseComponentsModule } from '../bases/components/base-components.module';

const CUSTOM_DATE_FORMATS: NgxMatDateFormats = {
  parse: {
    dateInput: 'l, LTS',
  },
  display: {
    dateInput: 'YYYY-MM-DD HH:mm:ss',
    monthYearLabel: 'MMM YYYY',
    dateA11yLabel: 'LL',
    monthYearA11yLabel: 'MMMM YYYY',
  },
};

@NgModule({
  declarations: [
    SeguimientoComponent,
    CronogramaComponent,
    ModalRegistroComponent
  ],
  imports: [
    MatButtonModule,
    NgxMatMomentModule,
    MatNativeDateModule,
    MatDatepickerModule,
    NgxMatDatetimePickerModule,
    NgxMatTimepickerModule,
    NgxMatNativeDateModule,
    FullCalendarModule,
    CommonModule,
    SeguimientoRoutingModule,
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
    NbCalendarRangeModule,
    NbTreeGridModule,
    BaseComponentsModule
  ],
  providers: [{ provide: NGX_MAT_DATE_FORMATS, useValue: CUSTOM_DATE_FORMATS }],
})
export class SeguimientoModule {}
