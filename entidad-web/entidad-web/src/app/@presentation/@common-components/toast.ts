import { Injectable } from '@angular/core';
import { NbComponentStatus, NbToastrService } from '@nebular/theme';

@Injectable({
  providedIn: 'root',
})
export class ToastService {
  constructor(private toastrService: NbToastrService) {}

  showToast(message: string, status: NbComponentStatus, title: string = null) {
    if (title == null) {
      switch (status) {
        case 'success':
          title = 'Correcto';
          break;
        case 'info':
          title = 'Información';
          break;
        case 'warning':
          title = 'Advertencia';
          break;
        case 'danger':
          title = 'Error';
          break;
        default:
          break;
      }
    }

    setTimeout(() => {
      this.toastrService.show(`${message}`, title || 'Atención', {
        status,
        destroyByClick: false,
      });
    }, 100);
  }
}
