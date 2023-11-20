import { NgModule } from '@angular/core';
import { CronogramaComponent } from './cronograma.component';
import { CronogramaRoutingModule } from './cronograma-routing.module';
import { CommonModule } from '@angular/common';
import { ThemeModule } from '../../@theme/theme.module';
import { FormsModule, ReactiveFormsModule } from '@angular/forms';
import {
  NbActionsModule,
  NbAutocompleteModule,
  NbButtonModule,
  NbDatepickerModule,
  NbFormFieldModule,
  NbIconModule,
  NbInputModule,
  NbLayoutModule, NbPopoverModule,
  NbSelectModule, NbTreeGridModule
} from '@nebular/theme';
import { CommonComponentsModule } from '../../@common-components/common-components.module';
import { MatDividerModule } from '@angular/material/divider';
import { NgxTrimDirectiveModule } from 'ngx-trim-directive';
import { MatTabsModule } from '@angular/material/tabs';
import { MatButtonModule } from '@angular/material/button';
import { MatPaginatorModule } from '@angular/material/paginator';
import { MatChipsModule } from '@angular/material/chips';
import { MatIconModule } from '@angular/material/icon';
import { MatAutocompleteModule } from '@angular/material/autocomplete';
import { MatProgressBarModule } from '@angular/material/progress-bar';
import { NgxSpinnerModule } from 'ngx-spinner';
import { MatListModule } from '@angular/material/list';
import { MatTableModule } from '@angular/material/table';
import { MatSortModule } from '@angular/material/sort';
import { MatRippleModule } from '@angular/material/core';
import { MatDialogModule } from '@angular/material/dialog';
import { MatCardModule } from '@angular/material/card';
import { ModalRegistrarActComponent } from './modal-registrar-act/modal-registrar-act.component';
import { ModalResolucionComponent } from './modal-resolucion/modal-resolucion.component';
import { HistorialCronogramaComponent } from './historial-cronograma/historial-cronograma.component';
import { ModalRemoveCronogramaComponent } from './modal-remove-cronograma/modal-remove-cronograma.component';
import { NgxDropzoneModule } from 'ngx-dropzone';

@NgModule({
  declarations: [
    CronogramaComponent,
    ModalRegistrarActComponent,
    ModalResolucionComponent,
    HistorialCronogramaComponent,
    ModalRemoveCronogramaComponent,
  ],
  imports: [
    CommonModule,
    CronogramaRoutingModule,
    ThemeModule,
    FormsModule,
    NbButtonModule,
    ReactiveFormsModule,
    NbIconModule,
    CommonComponentsModule,
    NbLayoutModule,
    MatDividerModule,
    NbSelectModule,
    NbInputModule,
    NbFormFieldModule,
    NbAutocompleteModule,
    NgxTrimDirectiveModule,
    MatTabsModule,
    NbPopoverModule,
    MatButtonModule,
    MatPaginatorModule,
    NbTreeGridModule,
    MatChipsModule,
    MatIconModule,
    MatAutocompleteModule,
    MatProgressBarModule,
    NgxSpinnerModule,
    MatListModule,
    MatTableModule,
    MatSortModule,
    MatRippleModule,
    MatDialogModule,
    MatCardModule,
    NbDatepickerModule,
    NbActionsModule,
    NgxDropzoneModule
  ]
})
export class CronogramaModule {}
