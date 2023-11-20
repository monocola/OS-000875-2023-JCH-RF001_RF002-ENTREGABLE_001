import { NgModule } from '@angular/core';
import { ParticipantesComponent } from './participantes.component';
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
  NbRadioModule,
  NbSelectModule,
  NbTabsetModule,
  NbTreeGridModule
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
import { MatCardModule } from '@angular/material/card';
import { NgxDropzoneModule } from 'ngx-dropzone';
import { MatDialogModule } from '@angular/material/dialog';
import { MatFormFieldModule } from '@angular/material/form-field';
import { ParticipantesRoutingModules } from './participantes-routing.modules';
import { EvaluadosComponent } from './evaluados/evaluados.component';
import { PaginationModule } from 'ngx-bootstrap/pagination';
import { AsignarMandoMedioComponent } from './asignar-mando-medio/asignar-mando-medio.component';
import { ModalEliminarEvaluadoComponent } from './modal-eliminar-evaluado/modal-eliminar.component';
import { AgregarEvaluadosComponent } from './agregar-evaluados/agregar-evaluados.component';
import { AgregarEvaluadosMmComponent } from './agregar-evaluados-mm/agregar-evaluados-mm.component';
import { MetasComponent } from './metas/metas.component';
import { AgregarMetaComponent } from './agregar-meta/agregar-meta.component';
import {MatRadioModule} from '@angular/material/radio';
import { NuevaEvidenciaComponent } from './nueva-evidencia/nueva-evidencia.component';
import { EditarEvidenciaComponent } from './editar-evidencia/editar-evidencia.component';
import { EliminarEvidenciaComponent } from './eliminar-evidencia/eliminar-evidencia.component';
import { ModalDocumentoComponent } from './modal-documento/modal-documento.component';
import { ValidarMetaComponent } from './validar-meta/validar-meta.component';
import { ObservarMetaComponent } from './observar-meta/observar-meta.component';
import { EditarMetaComponent } from './editar-meta/editar-meta.component';
import { ModalMsjEnvioDeCorreoComponent } from './modal-msj-envio-de-correo/modal-msj-envio-de-correo.component';
import { MatInputModule } from '@angular/material/input';
import { ModalBtnHabilitarEdicionComponent } from './modal-btn-habilitar-edicion/modal-btn-habilitar-edicion.component';
import { MatCheckboxModule } from '@angular/material/checkbox';
import { ModalEliminarMetaComponent } from './modal-eliminar-meta/modal-eliminar-meta.component';
import { FilterPPipe } from './pipes/filter-p.pipe';
import {IvyCarouselModule} from 'angular-responsive-carousel';


@NgModule({
  declarations: [
    ParticipantesComponent,
    EvaluadosComponent,
    AsignarMandoMedioComponent,
    ModalEliminarEvaluadoComponent,
    AgregarEvaluadosComponent,
    AgregarEvaluadosMmComponent,
    MetasComponent,
    AgregarMetaComponent,
    NuevaEvidenciaComponent,
    EditarEvidenciaComponent,
    EliminarEvidenciaComponent,
    ModalDocumentoComponent,
    ValidarMetaComponent,
    ObservarMetaComponent,
    ModalMsjEnvioDeCorreoComponent,
    EditarMetaComponent,
    ModalBtnHabilitarEdicionComponent,
    ModalEliminarMetaComponent,
    FilterPPipe,
  ],
  imports: [
    CommonModule,
    ParticipantesRoutingModules,
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
    PaginationModule,
    NbRadioModule,
    MatRadioModule,
    NbActionsModule,
    MatInputModule,
    MatCheckboxModule,
    IvyCarouselModule,

  ]
})
export class ParticipantesModule {}
