import { NgModule } from '@angular/core';
import { OrganigramaComponent } from './organigrama.component';
import { CommonModule } from '@angular/common';
import { OrganigramaRoutingModule } from './organigrama-routing.module';
import { ThemeModule } from '../../@theme/theme.module';
import {
  NbActionsModule,
  NbAutocompleteModule,
  NbButtonModule,
  NbFormFieldModule,
  NbIconModule, NbInputModule,
  NbLayoutModule, NbPopoverModule,
  NbSelectModule, NbTreeGridModule
} from '@nebular/theme';
import { FormsModule, ReactiveFormsModule } from '@angular/forms';
import { CommonComponentsModule } from '../../@common-components/common-components.module';
import { MatDividerModule } from '@angular/material/divider';
import { NgxTrimDirectiveModule } from 'ngx-trim-directive';
import { MatTabsModule } from '@angular/material/tabs';
import { MatCardModule } from '@angular/material/card';
import { MatPaginatorModule } from '@angular/material/paginator';
import { MatButtonModule } from '@angular/material/button';
import { MatChipsModule } from '@angular/material/chips';
import { MatIconModule } from '@angular/material/icon';
import { MatAutocompleteModule } from '@angular/material/autocomplete';
import { MatProgressBarModule } from '@angular/material/progress-bar';
import { NgxSpinnerModule } from 'ngx-spinner';
import { MatListModule } from '@angular/material/list';
import { MatTableModule } from '@angular/material/table';
import { MatSortModule } from '@angular/material/sort';
import { MatRippleModule } from '@angular/material/core';
import { VistaComponent } from './vista/vista.component';
import { GraphComponent } from './vista/graph/graph.component';
import { NgxDropzoneModule } from 'ngx-dropzone';
import { ModalConfirmacionComponent } from './modal-confirmacion/modal-confirmacion.component';
import { ModalConfirmacionTodoBienComponent } from './modal-confirmacion-todo-bien/modal-confirmacion-todo-bien.component';
import { MatDialogModule } from '@angular/material/dialog';
import { FDropzoneOrgComponent } from './f-dropzone-org/f-dropzone-org.component';
import { ModalCreacionUoComponent } from './modal-creacion-uo/modal-creacion-uo.component';
import { ModalRemoveComponent } from './modal-remove/modal-remove.component';


@NgModule({
  declarations: [
    OrganigramaComponent,
    VistaComponent,
    GraphComponent,
    ModalConfirmacionComponent,
    ModalConfirmacionTodoBienComponent,
    FDropzoneOrgComponent,
    ModalCreacionUoComponent,
    ModalRemoveComponent,
  ],
  imports: [
    CommonModule,
    OrganigramaRoutingModule,
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
    NgxDropzoneModule,
    MatDialogModule,
    NbActionsModule,
    MatCardModule
  ]
})
export class OrganigramaModule {}
