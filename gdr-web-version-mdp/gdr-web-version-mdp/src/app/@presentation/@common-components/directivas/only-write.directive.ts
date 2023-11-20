import { Directive, ElementRef, OnInit } from '@angular/core';
import { PermissionRepository } from '../../../@domain/repository/permission.repository';

@Directive({
  selector: '[onlyWrite]'
})
export class OnlyWriteDirective implements OnInit {

  constructor(private elementRef: ElementRef, private permissionRepository: PermissionRepository) {

  }

  ngOnInit(): void {
    if (this.permissionRepository.isOnlyRead) {
      try {
        this.elementRef.nativeElement.style.display = 'none';
      } catch (e) {
        console.log("Directiva onlyWrite: Error a ocultar elemento");
      }
    }
  }
}
