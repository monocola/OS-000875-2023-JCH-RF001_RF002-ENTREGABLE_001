import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';
import { SolicitudExtComponent } from './solicitud-ext.component';
import { SolicitudExtRoutingModule } from './solicitud-ext-routing.module';
import { ThemeModule } from '../../@theme/theme.module';
import {
  NbActionsModule,
  NbAutocompleteModule,
  NbButtonModule, NbCardModule, NbDatepickerModule,
  NbFormFieldModule,
  NbIconModule,
  NbInputModule,
  NbLayoutModule, NbPopoverModule,
  NbSelectModule, NbTabsetModule, NbTreeGridModule
} from '@nebular/theme';
import { FormsModule, ReactiveFormsModule } from '@angular/forms';
import { MatDividerModule } from '@angular/material/divider';
import { NgxTrimDirectiveModule } from 'ngx-trim-directive';
import { MatTabsModule } from '@angular/material/tabs';
import { MatButtonModule } from '@angular/material/button';
import { MatPaginatorModule } from '@angular/material/paginator';
import { MatChipsModule } from '@angular/material/chips';
import { MatIconModule } from '@angular/material/icon';
import { MatAutocompleteModule } from '@angular/material/autocomplete';
import { MatTableModule } from '@angular/material/table';
import { MatSortModule } from '@angular/material/sort';
import { MatRippleModule } from '@angular/material/core';
import { MatCardModule } from '@angular/material/card';
import { NgxDropzoneModule } from 'ngx-dropzone';
import { MatDialogModule } from '@angular/material/dialog';
import { MatFormFieldModule } from '@angular/material/form-field';
import { CommonComponentsModule } from '../../@common-components/common-components.module';
import { DetalleSolicitudExtComponent } from './detalle-solicitud-ext/detalle-solicitud-ext.component';
import { ModalEliminarQuestionComponent } from './detalle-solicitud-ext/modal-detalle-eliminar-question/modal-eliminar-question.component';
import { ModalObservarSolicitudComponent } from './detalle-solicitud-ext/modal-observar-solicitud/modal-observar-solicitud.component';
import { NotificarSolicitudExtComponent } from './notificar-solicitud-ext/notificar-solicitud-ext.component';


@NgModule({
  declarations: [SolicitudExtComponent, DetalleSolicitudExtComponent, ModalEliminarQuestionComponent, ModalObservarSolicitudComponent, NotificarSolicitudExtComponent],
  imports: [
    CommonModule,
    SolicitudExtRoutingModule,
    ThemeModule,
    NbButtonModule,
    FormsModule,
    ReactiveFormsModule,
    NbIconModule,
    CommonComponentsModule,
    NbLayoutModule,
    MatDividerModule,
    NbSelectModule,
    NbInputModule,
    ReactiveFormsModule,
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
    NbCardModule,
    NbTabsetModule,
    NbActionsModule,
    NbDatepickerModule,
    MatTableModule,
    MatSortModule,
    MatRippleModule,
    NbCardModule,
    NbTabsetModule,
    MatCardModule,
    NgxDropzoneModule,
    MatDialogModule,
    MatFormFieldModule
  ]
})
export class SolicitudExtModule { }
