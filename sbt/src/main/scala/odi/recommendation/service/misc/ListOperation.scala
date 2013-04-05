package odi.recommendation

trait ListOperation {
  def addToList[A](userList: Option[List[A]], user: A): List[A] = {
    if(userList != None) {
      userList.get :+ user
    }
    else {
      List(user)
    }
  }

}
