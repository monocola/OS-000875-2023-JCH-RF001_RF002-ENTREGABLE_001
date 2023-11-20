import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';
import { SeguimientoProcesoComponent } from './seguimiento-proceso.component';
import { SeguimientoProcesoRoutingModule } from './seguimiento-proceso-routing.module';
import { MatDividerModule } from '@angular/material/divider';
import { ReactiveFormsModule } from '@angular/forms';
import {
  NbButtonModule,
  NbDatepickerModule,
  NbFormFieldModule,
  NbIconModule,
  NbInputModule,
  NbSelectModule,
  NbTreeGridModule,
} from '@nebular/theme';
import { TableSeguimientoComponent } from './table-seguimiento/table-seguimiento.component';
import { MatPaginatorModule } from '@angular/material/paginator';
import { MatTableModule } from '@angular/material/table';
import { MatSortModule } from '@angular/material/sort';
import { MatRippleModule } from '@angular/material/core';
import { CommonComponentsModule } from '../../@common-components/common-components.module';
import { MatButtonModule } from '@angular/material/button';
import {
  NgxMatDatetimePickerModule,
  NgxMatTimepickerModule,
  NgxMatNativeDateModule,
  NGX_MAT_DATE_FORMATS,
  NgxMatDateFormats,
} from '@angular-material-components/datetime-picker';
import { FullCalendarModule } from '@fullcalendar/angular';

const INTL_DATE_INPUT_FORMAT = {
  year: 'numeric',
  month: 'numeric',
  day: 'numeric',
  hourCycle: 'h12',
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
  declarations: [SeguimientoProcesoComponent, TableSeguimientoComponent],
  imports: [
    FullCalendarModule,
    NgxMatDatetimePickerModule,
    NgxMatTimepickerModule,
    NgxMatNativeDateModule,
    CommonModule,
    SeguimientoProcesoRoutingModule,
    MatDividerModule,
    ReactiveFormsModule,
    NbFormFieldModule,
    NbInputModule,
    NbIconModule,
    NbSelectModule,
    NbButtonModule,
    NbDatepickerModule,
    MatPaginatorModule,
    MatTableModule,
    MatSortModule,
    MatRippleModule,
    CommonComponentsModule,
    MatButtonModule,
    NbTreeGridModule,
  ],
  providers: [{ provide: NGX_MAT_DATE_FORMATS, useValue: MAT_DATE_FORMATS }],
})
export class SeguimientoProcesoModule {}
