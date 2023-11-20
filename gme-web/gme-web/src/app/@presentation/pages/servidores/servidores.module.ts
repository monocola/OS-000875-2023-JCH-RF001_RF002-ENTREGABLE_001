import { NgModule } from '@angular/core';
import { ServidoresComponent } from './servidores.component';
import { CommonModule } from '@angular/common';
import { ThemeModule } from '../../@theme/theme.module';
import {
  NbActionsModule,
  NbAutocompleteModule,
  NbButtonModule,
  NbCardModule,
  NbDatepickerModule,
  NbFormFieldModule,
  NbIconModule,
  NbInputModule,
  NbLayoutModule,
  NbPopoverModule,
  NbSelectModule,
  NbTabsetModule,
  NbTreeGridModule,
} from '@nebular/theme';
import { FormsModule, ReactiveFormsModule } from '@angular/forms';
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
import { ServidoresRoutingModules } from './servidores-routing.modules';
import { MatCardModule } from '@angular/material/card';
import { NgxDropzoneModule } from 'ngx-dropzone';
import { MatDialogModule } from '@angular/material/dialog';
import { ModalConfirmacionComponent } from './modal-confirmacion/modal-confirmacion.component';
import { MatFormFieldModule } from '@angular/material/form-field';
import { ModalConfirmacionTodoBienComponent } from './modal-confirmacion-todo-bien/modal-confirmacion-todo-bien.component';
import { FDropzoneComponent } from './f-dropzone/f-dropzone.component';
import { ModalServidorComponent } from './modal-servidor/modal-servidor.component';
import { ModalEliminarComponent } from './modal-eliminar/modal-eliminar.component';
import { ModalCeseComponent } from './modal-cese/modal-cese.component';
import { EditarServidorComponent } from './editar-servidor/editar-servidor.component';
import { EditarPuestoComponent } from './editar-puesto/editar-puesto.component';

@NgModule({
  declarations: [
    ServidoresComponent,
    ModalConfirmacionComponent,
    ModalConfirmacionTodoBienComponent,
    FDropzoneComponent,
    ModalServidorComponent,
    ModalEliminarComponent,
    ModalCeseComponent,
    EditarServidorComponent,
    EditarPuestoComponent
  ],
  imports: [
    CommonModule,
    ServidoresRoutingModules,
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
    MatProgressBarModule,
    NgxSpinnerModule,
    MatListModule,
    MatTableModule,
    MatSortModule,
    MatRippleModule,
    NbCardModule,
    NbTabsetModule,
    MatCardModule,
    NgxDropzoneModule,

    MatDialogModule,

    MatFormFieldModule,
    NbDatepickerModule,
    NbActionsModule,
  ],
})
export class ServidoresModule {}
