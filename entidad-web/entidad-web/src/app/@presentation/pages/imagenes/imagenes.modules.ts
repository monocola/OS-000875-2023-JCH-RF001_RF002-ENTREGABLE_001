import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';

import {
  NbButtonModule,
  NbFormFieldModule,
  NbIconModule,
  NbInputModule,
  NbOptionModule,
  NbSelectModule,
} from '@nebular/theme';
import { MatDividerModule } from '@angular/material/divider';
import { ReactiveFormsModule } from '@angular/forms';
import { NgxTrimDirectiveModule } from 'ngx-trim-directive';
import { CommonComponentsModule } from '../../@common-components/common-components.module';
import { ImagenesRoutingModule } from './imagenes-routing.module';
import { ImagenesComponent } from './imagenes.component';
import { NgbModule } from '@ng-bootstrap/ng-bootstrap';
import { ModalLinkComponent } from './modal-link/modal-link.component';
import { DragDropModule } from '@angular/cdk/drag-drop';

@NgModule({
  declarations: [ImagenesComponent, ModalLinkComponent],
  imports: [
    DragDropModule,
    NgbModule,
    CommonModule,
    ImagenesRoutingModule,
    MatDividerModule,
    NbButtonModule,
    NbFormFieldModule,
    NbOptionModule,
    NbSelectModule,
    CommonComponentsModule,
    ReactiveFormsModule,
    NbIconModule,
    NbInputModule,
    NgxTrimDirectiveModule,
  ],
})
export class ImagenModule {}
