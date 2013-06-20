package odi.recommendation

/** a trait is an interface with implementation
  * this trait takes an entry of a list if it exists it adds a value if not it creates a new list
  */
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
